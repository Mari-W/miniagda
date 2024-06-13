use crate::{
  diagnostics::{
    error::{ElabErr, Error},
    span::{Span, Spanned},
  },
  elaboration::eval::{eq, eval},
  syntax::{
    core::{Ctx, Decl, Prog, Tel},
    Ident,
  },
  trace,
};

use crate::syntax::core::{Set, Tm, TmAbs, TmAll, TmApp, Val, ValAll};

use self::{data::elab_data, func::elab_func, state::State};
use crate::diagnostics::Result;

mod eval;
mod state;

// -----------------------------------------------------------------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------------------------------------------------------------

pub fn elaborate(prog: Prog) -> Result<()> {
  let mut state = State::default();
  elab_prog(prog, &mut state)
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Elaboration
// -----------------------------------------------------------------------------------------------------------------------------------

// Programs
fn elab_prog(prog: Prog, state: &mut State) -> Result<()> {
  prog.decls.into_iter().map(|decl| elab_decl(decl, state)).collect::<Result<Vec<_>>>()?;
  Ok(())
}

fn elab_decl(decl: Decl, state: &mut State) -> Result<()> {
  state.forget(|state| match decl {
    Decl::Data(data) => elab_data(data, state),
    Decl::Func(func) => elab_func(func, state),
  })
}

// Data Types
mod data;

// Functions
mod func;

// Terms

fn elab_tm_chk(tm: Tm, ty: Val, state: &State) -> Result<()> {
  trace!("check that term `{}` has type `{}` (up to β-η reduction)", tm, ty);
  let tm_fmt = format!("{tm}");
  let ty_fmt = format!("{ty}");

  match (tm, ty) {
    (Tm::Abs(TmAbs { ident, ty: _, body, .. }), Val::All(ValAll { dom, codom, mut env, .. })) => {
      let mut n_state = state.clone();
      n_state.bind(ident.name, *dom);
      elab_tm_chk(*body, eval(codom, &env.ext_lvl(state.lvl)), &n_state)?;
    }
    (tm, ty) => {
      let ity = elab_tm_inf(tm, state)?;
      match eq(ity.clone(), ty.clone(), state.lvl) {
        Ok(()) => (),
        Err((v1, v2)) => return Err(Error::from(ElabErr::TypeMismatch { ty1: ity, ty2: ty, v1, v2 })),
      }
    }
  };
  trace!("checked that term `{}` has type `{}` (up to β-η reduction)", tm_fmt, ty_fmt);
  Ok(())
}

fn elab_tm_inf(tm: Tm, state: &State) -> Result<Val> {
  let tm_fmt = format!("{tm}");
  let ty = match tm {
    Tm::Var(x) => state.resolve(&x),
    Tm::Data(x) | Tm::Cstr(x) | Tm::Func(x) => state.resolve_global(&x),
    Tm::App(TmApp { left, right, .. }) => {
      let left_clone = *left.clone(); // TODO: performance
      let lty = elab_tm_inf(*left, state)?;
      match lty {
        Val::All(ValAll { dom, codom, mut env, .. }) => {
          elab_tm_chk(*right.clone(), *dom, state)?;
          let right = eval(*right, &state.env);
          eval(codom, &env.ext(right))
        }
        ty => return Err(Error::from(ElabErr::FunctionTypeExpected { tm: left_clone, got: ty })),
      }
    }
    tm @ Tm::Abs(_) => return Err(Error::from(ElabErr::AttemptAbsInfer { tm })),
    Tm::All(TmAll { ident, box dom, box codom, span }) => match elab_tm_inf(dom.clone(), state)? {
      Val::Set(Set { level: level1, .. }) => {
        let mut n_state = state.clone();
        n_state.bind(ident.name, eval(dom, &state.env));
        match elab_tm_inf(codom, &n_state)? {
          Val::Set(Set { level: level2, .. }) => Val::Set(Set { level: level1.max(level2), span }),
          tm => return Err(Error::from(ElabErr::ExpectedSetAll { got: tm })),
        }
      }
      tm => return Err(Error::from(ElabErr::ExpectedSetAll { got: tm })),
    },
    Tm::Set(Set { level, span }) => Val::Set(Set { level: level + 1, span }),
  };
  trace!("inferred type of `{}` to be `{}`", tm_fmt, ty);
  Ok(ty)
}

fn unroll_app(tm: Tm) -> Vec<Tm> {
  match tm {
    Tm::App(TmApp { box left, box right, .. }) => {
      let mut tms = unroll_app(left);
      tms.push(right);
      tms
    }
    tm => vec![tm],
  }
}

fn roll_app(left: Tm, tms: &[Tm]) -> Tm {
  if tms.is_empty() {
    return left;
  }
  let left_span = left.span();
  roll_app(
    Tm::App(TmApp {
      left: Box::new(left),
      right: Box::new(tms[0].clone()),
      span: Span {
        file: left_span.file,
        start: left_span.start,
        end: tms[0].span().end,
      },
    }),
    &tms[1..],
  )
}

// ------------------------------------------------------------------------------------------------------------------------------------
// Contexts

fn ctx_to_fn(ctx: &Ctx, end: Tm) -> Tm {
  tms_to_fn(&ctx.tms, &ctx.binds, end)
}

fn tel_to_fn(tel: &Tel, end: Tm) -> Tm {
  tms_to_fn(&tel.tms, &tel.binds, end)
}

fn tms_to_fn(tms: &[Tm], binds: &[Ident], end: Tm) -> Tm {
  if tms.is_empty() {
    assert!(binds.is_empty(), "ice: invariant that there are as many tms and tys are present was not met");
    return end;
  }
  let dom = tms[0].clone();
  let ident = binds[0].clone();
  let codom = tms_to_fn(&tms[1..], &binds[1..], end);
  let dom_span = dom.span();
  let codom_span = codom.span();
  Tm::All(TmAll {
    ident,
    dom: Box::new(dom),
    codom: Box::new(codom),
    span: Span {
      file: dom_span.file,
      start: dom_span.start,
      end: codom_span.end,
    },
  })
}

fn fn_to_ctx(ty: Tm) -> (Ctx, Tm) {
  let span = ty.span();
  let (binds, tms, r_ty) = fn_to_tms(ty);
  (Ctx { binds, tms, span }, r_ty)
}


fn fn_to_tel(ty: Tm) -> (Tel, Tm) {
  let span = ty.span();
  let (binds, tms, r_ty) = fn_to_tms(ty);
  (Tel { binds, tms, span }, r_ty)
}

fn fn_to_tel_cut(ty: Tm, cut: usize) -> (Tel, Tm) {
  let span = ty.span();
  let (binds, tms, r_ty) = fn_to_tms(ty);
  (Tel { binds, tms, span }, r_ty)
}

fn fn_to_tms(ty: Tm) -> (Vec<Ident>, Vec<Tm>, Tm) {
  match ty {
    Tm::Abs(_) => unreachable!("ice: there should not be an abstraction in a function type at this point"),
    Tm::All(TmAll { ident, box dom, box codom, .. }) => {
      let (mut binds, mut tms, rty) = fn_to_tms(codom);
      binds.insert(0, ident);
      tms.insert(0, dom);
      (binds, tms, rty)
    }
    tm => (vec![], vec![], tm),
  }
}
