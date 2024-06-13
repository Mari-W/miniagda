use super::elab_tm_inf;
use super::eval::eval;
use super::state::State;
use crate::diagnostics::error::{ElabErr, Error};
use crate::diagnostics::Result;
use crate::elaboration::eval::nf;
use crate::elaboration::state::DataInfo;
use crate::elaboration::{ctx_to_fn, elab_tm_chk, fn_to_tel, tel_to_fn, unroll_app};
use crate::syntax::core::{Env, Tm};
use crate::syntax::{
  core::{Cstr, Data, Set, TmVar, Val},
  Ident,
};
use crate::{debug, trace};

// -----------------------------------------------------------------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------------------------------------------------------------

pub fn elab_data(data: Data, state: &mut State) -> Result<()> {
  let data_clone = data.clone();

  let level = match data.set {
    Tm::Set(Set { level, .. }) => level,
    tm => return Err(Error::from(ElabErr::ExpectedSetData { got: tm })),
  };

  debug!("elaborating data type `{}`", data.ident);
  let as_fn = eval(ctx_to_fn(&data.params, tel_to_fn(&data.indices, data.set)), &Env::default());

  let param_len = data.params.binds.len();
  debug!("elaborating parameters `{}` of data type `{}`", data.params, data.ident);
  elab_params(data.params.binds, data.params.tms, None, state)?;

  debug!("elaborating indices `{}` of data type `{}`", data.indices, data.ident);
  let indices_types = state.forget(|state| elab_params(data.indices.binds, data.indices.tms, None, state))?;

  state.bind_global(data.ident.clone(), as_fn);
  // need this for pattern matching
  state.add_type_info(
    data.ident.clone(),
    DataInfo {
      params: param_len,
      is_empty: data.cstrs.is_empty(),
    },
  );

  data
    .cstrs
    .into_iter()
    .map(|cstr| state.forget(|env| elab_cstr(cstr, &data_clone, level, &indices_types, env)))
    .collect::<Result<Vec<_>>>()?;
  debug!("elaborated data type `{}`", data.ident);
  Ok(())
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Elaboration
// -----------------------------------------------------------------------------------------------------------------------------------

// Constructors

fn elab_cstr(cstr: Cstr, data: &Data, level: usize, indices_types: &[Val], state: &mut State) -> Result<()> {
  let name = cstr.ident.clone();
  debug!("elaborating constructor `{}`", name);

  let (args, r_ty) = fn_to_tel(nf(cstr.ty, &state.env));
  let rhs = unroll_app(r_ty.clone());
  trace!("unrolled return type of constructor {} to {:?}", r_ty, rhs);

  match &rhs[0] {
    Tm::Data(ident) if ident == &data.ident => (),
    tm => {
      return Err(Error::from(ElabErr::ExpectedData {
        expected: data.ident.clone(),
        got: tm.clone(),
      }))
    }
  }

  let as_fn = eval(ctx_to_fn(&data.params, tel_to_fn(&args, r_ty)), &Env::default());

  let params = &rhs[1..];

  let args_len = args.binds.len();

  debug!("elaborating constructor arguments `{}`", args);
  elab_params(args.binds, args.tms, Some(level), state)?;

  let data_params_len = data.params.binds.len();
  for i in 0..data_params_len {
    if let Some(Tm::Var(TmVar { idx, name: _, .. })) = params.get(i) {
      if idx.0 == data_params_len + args_len - (i + 1) {
        continue;
      }
    }
    return Err(Error::from(ElabErr::ExpectedParam {
      expected: data.params.binds[i].clone(),
      got: params.get(i).cloned(),
    }));
  }

  let indices = &params[data_params_len..];

  let data_indices_len = data.indices.binds.len();
  for i in 0..data_indices_len {
    if indices.get(i).is_some() {
      continue;
    }

    return Err(Error::from(ElabErr::ExpectedIndex {
      expected: data.indices.binds[i].clone(),
      got: indices.get(i).cloned(),
    }));
  }
  if let Some(tm) = indices.get(data_indices_len) {
    return Err(Error::from(ElabErr::UnexpectedArg { got: tm.clone() }));
  }

  debug!(
    "checking constructor indices `[{}]` match expected types `[{}]`",
    indices.iter().map(|x| format!("{x}")).collect::<Vec<String>>().join(", "),
    data.indices.tms.iter().map(|x| format!("{x}")).collect::<Vec<String>>().join(", ")
  );
  elab_indices(indices, indices_types, &data.indices.binds, state)?;

  state.bind_global(cstr.ident, as_fn);

  // TODO: termination checking

  debug!("elaborated constructor `{}`", name);
  Ok(())
}

// Parameters

fn elab_params(binds: Vec<Ident>, tms: Vec<Tm>, max_lvl: Option<usize>, state: &mut State) -> Result<Vec<Val>> {
  pub fn expected_set(ty: &Val, max_lvl: Option<usize>) -> Result<()> {
    if let Val::Set(Set { level, .. }) = ty {
      if let Some(max_lvl) = max_lvl {
        if *level > max_lvl {
          return Err(Error::from(ElabErr::LevelTooHigh { tm: ty.clone(), max: max_lvl }));
        }
      }
      return Ok(());
    }
    Err(Error::from(ElabErr::ExpectedSetCtx { got: ty.clone() }))
  }

  binds
    .into_iter()
    .zip(tms)
    .map(|(Ident { name, .. }, tm)| {
      let ty = elab_tm_inf(tm.clone(), state)?;
      expected_set(&ty, max_lvl)?;
      let tm = eval(tm, &state.env);
      state.bind(name, tm.clone());
      Ok(tm)
    })
    .collect::<Result<Vec<_>>>()
}

// Indices

fn elab_indices(tms: &[Tm], tys: &[Val], binds: &[Ident], state: &State) -> Result<()> {
  assert!(
    tms.len() == binds.len() && binds.len() == tms.len(),
    "ice: invariant that there are as many tms and tys are present was not met"
  );
  if tms.is_empty() {
    return Ok(());
  }
  elab_tm_chk(tms[0].clone(), tys[0].clone(), state)?;
  if binds.len() > 1 {
    return elab_indices(&tms[1..], &tys[1..], &binds[1..], state);
  }
  Ok(())
}
