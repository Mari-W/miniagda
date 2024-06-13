use std::iter::once;

use crate::{
  debug,
  diagnostics::{
    error::{ElabErr, Error},
    span::Spanned,
  },
  elaboration::{eval::nf, fn_to_tel, fn_to_tel_cut, roll_app},
  syntax::{
    core::{Cls, ClsAbsurd, ClsClause, Env, Func, Lvl, Pat, PatCst, PatDot, PatVar, Tel, Tm, TmVar, Val},
    Ident,
  },
  trace,
};

use super::{elab_tm_inf, eval::quote, state::State, unroll_app};
use crate::diagnostics::Result;

// -----------------------------------------------------------------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------------------------------------------------------------

pub fn elab_func(func: Func, state: &mut State) -> Result<()> {
  assert!(state.env.0.is_empty(), "ice: did not expect variables in scope for type of function definition");

  debug!("elaborating function {}", func.ident);

  let n_ty = nf(func.ty.clone(), &state.env);
  trace!("normalized {} to {}", func.ty, n_ty);

  match elab_tm_inf(n_ty.clone(), state)? {
    Val::Set(_) => {}
    got => return Err(Error::from(ElabErr::ExpectedSetFun { got })),
  }

  if func.cls.is_empty() {
    return Err(Error::from(ElabErr::MissingFunctionBody { ident: func.ident.clone() }));
  }

  let pats = match &func.cls[0] {
    Cls::Cls(ClsClause { pats, .. }) => pats.len(),
    Cls::Abs(ClsAbsurd { pats, .. }) => pats.len(),
  };
  for cls in &func.cls[1..] {
    let len = match cls {
      Cls::Cls(ClsClause { pats, .. }) => pats.len(),
      Cls::Abs(ClsAbsurd { pats, .. }) => pats.len(),
    };
    if len != pats {
      return Err(Error::from(ElabErr::MismatchPatAmount {
        expected: pats,
        got: len,
        ident: func.ident.clone(),
        span: cls.span(),
      }));
    }
  }

  let (tel, r_ty) = fn_to_tel_cut(n_ty, pats);
  trace!("got arguments {} and return type {}", tel, r_ty);

  for cls in func.cls {
    elab_cls(cls, &tel, state)?;
  }

  todo!()
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Elaboration
// -----------------------------------------------------------------------------------------------------------------------------------

// Clauses

fn elab_cls(cls: Cls, tel: &Tel, state: &State) -> Result<()> {
  let cls_cpy = cls.clone();
  trace!("inferring tel for clause {cls} in tel {tel}");
  match cls {
    Cls::Cls(ClsClause { pats, span, .. }) => {
      let (tel_binds, tel_tms, tms) = infer_pats(pats, tel, state)?;
      let tel = Tel {
        binds: tel_binds,
        tms: tel_tms,
        span,
      };
      trace!("inferred tel {tel} for clause {cls_cpy} and terms {tms:?}")
    }
    Cls::Abs(ClsAbsurd { .. }) => {}
  }

  Ok(())
}

// Patterns

fn infer_pats(pats: Vec<Pat>, tel: &Tel, state: &State) -> Result<(Vec<Ident>, Vec<Tm>, Vec<Tm>)> {
  if pats.len() > tel.binds.len() {
    return Err(Error::from(ElabErr::TooManyPatterns {
      expected: tel.binds.len(),
      got: pats.len(),
      span: pats.last().unwrap().span(),
    }));
  }
  Ok(
    pats
      .iter()
      .enumerate()
      .map(|(i, pat)| {
        trace!("infer tel and tm for pat {pat} in {tel}");
        infer_pat(pat, &tel.tms[i], state)
      })
      .collect::<Result<Vec<_>>>()?
      .into_iter()
      .fold((vec![], vec![], vec![]), |(tel_idents_acc, tel_tms_acc, tms_acc), (tel_idents, tel_tms, tm)| {
        (
          tel_idents_acc.into_iter().chain(tel_idents).collect(),
          tel_tms_acc.into_iter().chain(tel_tms).collect(),
          tms_acc.into_iter().chain(once(tm)).collect(),
        )
      }),
  )
}

fn infer_pat(pat: &Pat, ty: &Tm, state: &State) -> Result<(Vec<Ident>, Vec<Tm>, Tm)> {
  trace!("inferring tel and tm for pat {pat} of type {ty}");
  match pat {
    Pat::Var(PatVar { name, span, idx }) => Ok((
      vec![Ident {
        name: name.clone(),
        span: span.clone(),
      }],
      vec![ty.clone()],
      Tm::Var(TmVar {
        name: name.clone(),
        idx: *idx,
        span: span.clone(),
      }),
    )),
    Pat::Cst(PatCst { cstr, pats, span }) => {
      // get type and its params + the info how many of params there are
      let ty = unroll_app(ty.clone());
      let (ident, info) = match &ty[0] {
        Tm::Data(ident) => (ident, state.get_type_info(ident)),
        tm => return Err(Error::from(ElabErr::ExpectedDataForCstrPat { got: tm.clone() })),
      };

      assert!(
        ty.len() - 1 >= info.params,
        "ice: a type was checked to have kind set even though it was not fully applied to its parameters"
      );

      // apply cstr type to params implicitly and normalize
      let cstr_app = nf(roll_app(quote(Lvl(0), state.resolve_global(cstr)), &ty[1..=info.params]), &Env::default());
      let (tel, r_ty) = fn_to_tel(cstr_app.clone());

      // check that the constructor is of that type
      let r_ty = unroll_app(r_ty);
      match &r_ty[0] {
        Tm::Data(data) if ident != data => {
          return Err(Error::from(ElabErr::CstrNotPresent {
            data: ident.clone(),
            got: cstr.clone(),
          }));
        }
        Tm::Data(_) => (),
        _ => panic!("ice: constructor is checked to definitely end in data type here"),
      }

      // check that there are as many patterns as types
      if pats.len() != tel.binds.len() {
        return Err(Error::from(ElabErr::MismatchPatAmount {
          expected: tel.binds.len(),
          got: pats.len(),
          ident: cstr.clone(),
          span: span.clone(),
        }));
      }

      // recurse
      let (binds, tms, args) = infer_pats(pats.clone(), &tel, state)?;
      Ok((binds, tms, roll_app(cstr_app, &args)))
    }
    Pat::Dot(PatDot { tm, .. }) => Ok((vec![], vec![], tm.clone())),
  }
}
