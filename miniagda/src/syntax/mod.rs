use crate::{
  diagnostics::Result,
  diagnostics::{
    error::{Error, TransErr},
    span::Span,
  },
  syntax::{core::PatCst, surf::PatId},
};
use std::{fmt::Display, hash::Hash};

use self::core::{ClsAbsurd, ClsClause};

// -----------------------------------------------------------------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------------------------------------------------------------

pub mod core;
pub mod surf;

pub fn translate(prog: surf::Prog) -> Result<core::Prog> {
  let mut env = Env::default();
  Ok(core::Prog {
    decls: prog.decls.into_iter().map(|decl| translate_decl(decl, &mut env)).collect::<Result<Vec<_>>>()?,
    span: prog.span,
  })
}

#[derive(Clone, Debug)]
pub struct Ident {
  pub name: String,
  pub span: Span,
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Translation
// -----------------------------------------------------------------------------------------------------------------------------------

// Env

#[derive(Clone, Debug)]
enum Glo {
  Cstr,
  Data,
  Func,
}

#[derive(Clone, Debug, Default)]
struct Env {
  var: Vec<Ident>,
  glo: Vec<(Ident, Glo)>,
}

impl Env {
  fn resolve_pat(&self, ident: &Ident) -> core::Idx {
    core::Idx(
      self
        .var
        .iter()
        .position(|n| n.name == ident.name)
        .expect("ice: did not correctly brought all pattern variables to scope"),
    )
  }

  fn resolve(&self, ident: Ident) -> Result<core::Tm> {
    assert!(ident.name != "_", "ice: term inference not yet implemented");
    match self.var.iter().position(|n| n.name == ident.name) {
      Some(idx) => Ok(core::Tm::Var(core::TmVar {
        name: ident.name,
        idx: core::Idx(idx),
        span: ident.span,
      })),
      None => {
        if let Some((_, glo)) = self.glo.iter().find(|(x, _)| x == &ident) {
          Ok(match glo {
            Glo::Cstr => core::Tm::Cstr(ident),
            Glo::Data => core::Tm::Data(ident),
            Glo::Func => core::Tm::Func(ident),
          })
        } else {
          Err(Error::from(TransErr::UnboundVariable { ident }))
        }
      }
    }
  }

  fn has_cstr(&self, var: &Ident) -> bool {
    matches!(self.glo.iter().find(|(x, _)| x == var), Some((_, Glo::Cstr)))
  }

  pub fn add_glo(&mut self, x: Ident, glo: Glo) -> Result<()> {
    if self.glo.iter().any(|(var, _)| &x == var) {
      return Err(Error::from(TransErr::DuplicatedGlobal { ident: x }));
    }
    self.glo.push((x, glo));
    Ok(())
  }

  pub fn add_var(&mut self, x: Ident) {
    self.var.insert(0, x);
  }

  pub fn forget<T>(&mut self, f: impl FnOnce(&mut Env) -> T) -> T {
    let len = self.var.len();
    let res = f(self);
    self.var.drain(0..(self.var.len() - len));
    res
  }
}

// Programs

fn translate_decl(decl: surf::Decl, env: &mut Env) -> Result<core::Decl> {
  env.forget::<Result<_>>(|env| {
    Ok(match decl {
      surf::Decl::Data(data) => core::Decl::Data(translate_data(data, env)?),
      surf::Decl::Func(func) => core::Decl::Func(translate_func(func, env)?),
    })
  })
}

// Data Types

fn translate_data(data: surf::Data, env: &mut Env) -> Result<core::Data> {
  let params = translate_ctx(data.params, env)?;

  let indices = env.forget(|env| translate_tel(data.indices, env))?;

  let set = translate_tm(data.set, &mut Env::default())?;

  env.add_glo(data.ident.clone(), Glo::Data)?;

  let cstrs = data.cstrs.into_iter().map(|cstr| translate_cstr(cstr, env)).collect::<Result<Vec<_>>>()?;

  Ok(core::Data {
    ident: data.ident,
    params,
    indices,
    set,
    cstrs,
    span: data.span,
  })
}

fn translate_cstr(cstr: surf::Cstr, env: &mut Env) -> Result<core::Cstr> {
  env.add_glo(cstr.ident.clone(), Glo::Cstr)?;

  Ok(core::Cstr {
    ident: cstr.ident,
    ty: translate_tm(cstr.ty, env)?,
    span: cstr.span,
  })
}

// Functions

fn translate_func(func: surf::Func, env: &mut Env) -> Result<core::Func> {
  let ty = translate_tm(func.ty, env)?;

  env.add_glo(func.ident.clone(), Glo::Func)?;

  let cls = func
    .cls
    .into_iter()
    .map(|cls| env.forget(|env| surface_to_core_cls(&func.ident, cls, env)))
    .collect::<Result<Vec<core::Cls>>>()?;

  Ok(core::Func {
    ident: func.ident,
    ty,
    cls,
    span: func.span,
  })
}

fn surface_to_core_cls(ident: &Ident, cls: surf::Cls, env: &mut Env) -> Result<core::Cls> {
  let (func, pats, span, rhs) = match cls {
    surf::Cls::Cls(surf::ClsClause { func, pats, rhs, span }) => (func, pats, span, Some(rhs)),
    surf::Cls::Abs(surf::ClsAbsurd { func, pats, span }) => (func, pats, span, None),
  };

  if ident != &func {
    return Err(Error::from(TransErr::FunctionNameExpected {
      function: ident.clone(),
      clause: func,
    }));
  }

  let pats = translate_pats(pats, env)?;
  match rhs {
    Some(rhs) => Ok(core::Cls::Cls(ClsClause {
      func,
      pats,
      rhs: translate_tm(rhs, env)?,
      span,
    })),
    None => Ok(core::Cls::Abs(ClsAbsurd { func, pats, span })),
  }
}

fn translate_pats(pats: Vec<surf::Pat>, env: &mut Env) -> Result<Vec<core::Pat>> {
  fn bring_variables_into_scope<'a>(pats: &'a [surf::Pat], env: &mut Env, in_scope: &mut Vec<&'a Ident>) -> Result<()> {
    pats
      .iter()
      .map(move |pat| {
        if let surf::Pat::Id(PatId { ident, pats, .. }) = pat {
          if env.has_cstr(ident) {
            bring_variables_into_scope(pats, env, in_scope)?;
          } else {
            if !pats.is_empty() {
              return Err(Error::from(TransErr::UnresolvedConstructor { ident: ident.clone() }));
            }
            if ident.name != "_" && in_scope.contains(&ident) {
              return Err(Error::from(TransErr::DuplicatedPatternVariable { ident: ident.clone() }));
            }
            env.add_var(ident.clone());
            in_scope.push(ident);
          }
        }
        Ok(())
      })
      .collect::<Result<Vec<_>>>()?;
    Ok(())
  }

  {
    let mut in_scope = Vec::new();
    bring_variables_into_scope(&pats, env, &mut in_scope)?;
  }

  pats
    .into_iter()
    .map(|pat| match pat {
      surf::Pat::Id(PatId { ident, pats, span }) => Ok(if env.has_cstr(&ident) {
        core::Pat::Cst(PatCst {
          cstr: ident,
          pats: translate_pats(pats, env)?,
          span,
        })
      } else {
        assert!(pats.is_empty(), "ice: only constructors should have sub patterns");
        let idx = env.resolve_pat(&ident);
        core::Pat::Var(core::PatVar { name: ident.name, idx, span })
      }),
      surf::Pat::Dot(surf::PatDot { tm, span }) => Ok(core::Pat::Dot(core::PatDot {
        tm: translate_tm(tm, env)?,
        span,
      })),
    })
    .collect::<Result<Vec<_>>>()
}

// Terms

fn translate_tm(tm: surf::Tm, env: &mut Env) -> Result<core::Tm> {
  Ok(match tm {
    surf::Tm::Var(x) => env.resolve(x)?,
    surf::Tm::App(surf::TmApp { left, right, span }) => core::Tm::App(core::TmApp {
      left: Box::new(translate_tm(*left, env)?),
      right: Box::new(translate_tm(*right, env)?),
      span,
    }),
    surf::Tm::Abs(surf::TmAbs { ident, ty, body, span }) => {
      let mut n_env: Env = env.clone();
      n_env.add_var(ident.clone());
      core::Tm::Abs(core::TmAbs {
        ident,
        ty: Box::new(translate_tm(*ty, env)?),
        body: Box::new(translate_tm(*body, &mut n_env)?),
        span,
      })
    }
    surf::Tm::All(surf::TmAll { ident, dom, codom, span }) => {
      let mut n_env = env.clone();
      n_env.add_var(ident.clone());
      core::Tm::All(core::TmAll {
        ident,
        dom: Box::new(translate_tm(*dom, env)?),
        codom: Box::new(translate_tm(*codom, &mut n_env)?),
        span,
      })
    }
    surf::Tm::Set(surf::TmSet { level, span }) => core::Tm::Set(core::Set { level, span }),
    surf::Tm::Brc(tm) => translate_tm(*tm, env)?,
  })
}

// Contexts

fn translate_tel(ctx: surf::Ctx, env: &mut Env) -> Result<core::Tel> {
  let (binds, tms) = translate_binds(ctx.binds, env)?;
  Ok(core::Tel { binds, tms, span: ctx.span })
}

fn translate_ctx(ctx: surf::Ctx, env: &mut Env) -> Result<core::Ctx> {
  let (binds, tms) = translate_binds(ctx.binds, env)?;
  Ok(core::Ctx { binds, tms, span: ctx.span })
}

fn translate_binds(binds: Vec<(Ident, surf::Tm)>, env: &mut Env) -> Result<(Vec<Ident>, Vec<core::Tm>)> {
  Ok(
    binds
      .into_iter()
      .map(|(x, tm)| {
        let tm = translate_tm(tm, env)?;
        env.add_var(x.clone());
        Ok((x, tm))
      })
      .collect::<Result<Vec<_>>>()?
      .into_iter()
      .unzip(),
  )
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Trait Impls
// -----------------------------------------------------------------------------------------------------------------------------------

impl PartialEq for Ident {
  fn eq(&self, other: &Self) -> bool {
    self.name == other.name
  }
}

impl Eq for Ident {}

impl Hash for Ident {
  fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
    self.name.hash(state);
  }
}

impl Display for Ident {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{}", self.name)
  }
}
