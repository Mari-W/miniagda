use crate::diagnostics::span::Spanned;
use crate::syntax::core::{Env, Idx, Lvl, Set, Tm, TmAbs, TmAll, TmApp, TmVar, Val, ValAbs, ValAll, ValApp, ValVar};
use crate::trace;

// -----------------------------------------------------------------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------------------------------------------------------------

// Evaluation

pub fn eval(tm: Tm, env: &Env) -> Val {
  let tm_str = format!("{tm}");
  let val = match tm {
    Tm::Var(x) => env.resolve(x),
    Tm::Data(x) => Val::Data(x),
    Tm::Cstr(x) => Val::Cstr(x),
    Tm::Func(x) => Val::Func(x),
    Tm::App(TmApp { left, right, span }) => match eval(*left, env) {
      Val::Abs(ValAbs { mut env, body, .. }) => {
        let right = eval(*right, &env);
        eval(body, &env.ext(right))
      }
      v => Val::App(ValApp {
        left: Box::new(v),
        right: Box::new(eval(*right, env)),
        span,
      }),
    },
    Tm::Abs(TmAbs { ident, ty, body, span }) => Val::Abs(ValAbs {
      env: env.clone(),
      ident,
      ty: Box::new(eval(*ty, env)),
      body: *body,
      span,
    }),
    Tm::All(TmAll { ident, dom, codom, span }) => Val::All(ValAll {
      env: env.clone(),
      ident,
      dom: Box::new(eval(*dom, env)),
      codom: *codom,
      span,
    }),
    Tm::Set(set) => Val::Set(set),
  };
  trace!("evaluated `{}` to `{}`", tm_str, val);
  val
}

// Quoting

pub fn quote(lvl: Lvl, val: Val) -> Tm {
  match val {
    Val::Var(ValVar { name, lvl: Lvl(x), span }) => {
      assert!(lvl.0 as isize - x as isize > 0, "ice: quoted from wrong level");
      Tm::Var(TmVar {
        name,
        idx: Idx(lvl.0 - x - 1),
        span,
      })
    }
    Val::Data(ident) => Tm::Data(ident),
    Val::Func(ident) => Tm::Func(ident),
    Val::Cstr(ident) => Tm::Cstr(ident),
    Val::App(ValApp { box left, box right, span }) => Tm::App(TmApp {
      left: Box::new(quote(lvl, left)),
      right: Box::new(quote(lvl, right)),
      span,
    }),
    Val::Abs(ValAbs {
      mut env,
      ident,
      box ty,
      body,
      span,
    }) => Tm::Abs(TmAbs {
      ident,
      ty: Box::new(quote(lvl, ty)),
      body: Box::new(quote(lvl + 1, eval(body, &env.ext_lvl(lvl)))),
      span,
    }),
    Val::All(ValAll {
      mut env,
      ident,
      box dom,
      codom,
      span,
    }) => Tm::All(TmAll {
      ident,
      dom: Box::new(quote(lvl, dom)),
      codom: Box::new(quote(lvl + 1, eval(codom, &env.ext_lvl(lvl)))),
      span,
    }),
    Val::Set(set) => Tm::Set(set),
  }
}

// Normal Form

pub fn nf(tm: Tm, env: &Env) -> Tm {
  quote(Lvl(env.0.len()), eval(tm, env))
}

// Equality

pub fn eq(ty1: Val, ty2: Val, lvl: Lvl) -> std::result::Result<(), (Val, Val)> {
  let ty1_fmt = format!("{ty1}");
  let ty2_fmt = format!("{ty2}");
  trace!("test for type equality of `{}` and `{}`", ty1_fmt, ty2_fmt,);
  match (ty1, ty2) {
    (Val::Set(Set { level: level1, .. }), Val::Set(Set { level: level2, .. })) if level1 == level2 => Ok(()),
    (
      Val::Abs(ValAbs {
        ty: ty1,
        body: body1,
        env: mut env1,
        ..
      }),
      Val::Abs(ValAbs {
        ty: ty2,
        body: body2,
        env: mut env2,
        ..
      }),
    ) => {
      eq(*ty1, *ty2, lvl)?;
      eq(eval(body1, &env1.ext_lvl(lvl)), eval(body2, &env2.ext_lvl(lvl)), lvl + 1)
    }
    (
      Val::All(ValAll {
        dom: dom1,
        codom: codom1,
        env: mut env1,
        ..
      }),
      Val::All(ValAll {
        dom: dom2,
        codom: codom2,
        env: mut env2,
        ..
      }),
    ) => {
      eq(*dom1, *dom2, lvl)?;
      eq(eval(codom1, &env1.ext_lvl(lvl)), eval(codom2, &env2.ext_lvl(lvl)), lvl + 1)
    }
    (Val::Abs(ValAbs { mut env, body, .. }), val) => {
      let var = Val::Var(ValVar::from(lvl));
      let span = val.span();
      eq(
        eval(body, &env.ext_lvl(lvl)),
        Val::App(ValApp {
          left: Box::new(val),
          right: Box::new(var),
          span,
        }),
        lvl + 1,
      )
    }
    (val, Val::Abs(ValAbs { mut env, body, .. })) => {
      let var = Val::Var(ValVar::from(lvl));
      let span = val.span();
      eq(
        Val::App(ValApp {
          left: Box::new(val),
          right: Box::new(var),
          span,
        }),
        eval(body, &env.ext_lvl(lvl)),
        lvl + 1,
      )
    }
    (Val::Var(ValVar { lvl: lvl1, .. }), Val::Var(ValVar { lvl: lvl2, .. })) if lvl1 == lvl2 => Ok(()),
    (Val::Data(ident1), Val::Data(ident2)) if ident1 == ident2 => Ok(()),
    (Val::Cstr(ident1), Val::Cstr(ident2)) if ident1 == ident2 => Ok(()),
    (Val::Func(ident1), Val::Func(ident2)) if ident1 == ident2 => Ok(()),
    (Val::App(ValApp { left: left1, right: right1, .. }), Val::App(ValApp { left: left2, right: right2, .. })) => {
      eq(*left1, *left2, lvl)?;
      eq(*right1, *right2, lvl)
    }
    (ty1, ty2) => Err((ty1, ty2)),
  }
}

// Eval Environment

impl Env {
  pub fn ext_lvl(&mut self, lvl: Lvl) -> Self {
    self.ext(Val::Var(ValVar::from(lvl)))
  }

  pub fn ext(&mut self, val: Val) -> Self {
    let mut env = self.to_owned();
    env.0.insert(0, val);
    env
  }

  fn resolve(&self, x: TmVar) -> Val {
    trace!(
      "resolving `{}` from env `[{}]`",
      x,
      self.0.iter().map(|v| format!("{v}")).collect::<Vec<String>>().join(", ")
    );
    match &self.0[x.idx.0] {
      Val::Var(ValVar { lvl, .. }) => Val::Var(ValVar {
        name: x.name,
        lvl: *lvl,
        span: x.span,
      }),
      v => v.clone(),
    }
  }
}
