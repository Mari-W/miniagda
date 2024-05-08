use std::collections::HashMap;

use crate::{
  debug,
  diagnostics::span::Span,
  syntax::{
    core::{Env, Lvl, TmVar, Val, ValVar},
    Ident,
  },
  trace,
};

#[derive(Clone, Debug)]
pub struct DataInfo {
  pub params: usize,
  pub is_empty: bool,
}

#[derive(Clone, Debug, Default)]
pub struct State {
  pub env: Env,
  pub types: Vec<Val>,
  pub global_types: HashMap<Ident, Val>,
  pub data_infos: HashMap<Ident, DataInfo>,
  pub lvl: Lvl,
}

impl State {
  pub fn resolve_global(&self, var: &Ident) -> Val {
    let ty = self.global_types.get(var).unwrap_or_else(|| panic!("could not resolve type of global {var}")).clone();
    trace!(
      "resolved global `{}` to be of type `{}` in `{{{}}}",
      var,
      ty,
      self.global_types.iter().map(|(k, v)| format!("{k} : {v}")).collect::<Vec<String>>().join(", ")
    );
    ty
  }

  pub fn resolve(&self, var: &TmVar) -> Val {
    let ty = self
      .types
      .get(var.idx.0)
      .unwrap_or_else(|| panic!("could not resolve type of variable {}", var.name))
      .clone();
    trace!(
      "resolved `{}` to be of type `{}` in [`{}`]",
      var,
      ty,
      self.types.iter().map(|v| format!("{v}")).collect::<Vec<String>>().join(", ")
    );
    ty
  }

  pub fn forget<T>(&mut self, f: impl FnOnce(&mut State) -> T) -> T {
    let len_tys = self.types.len();
    let len_env = self.env.0.len();
    let lvl = self.lvl;
    let res = f(self);
    self.types.drain(0..(self.types.len() - len_tys));
    self.env.0.drain(0..(self.env.0.len() - len_env));
    self.lvl = lvl;
    res
  }

  pub fn bind_global(&mut self, glo: Ident, ty: Val) {
    debug!("add global `{}` with type `{}`", glo, ty);
    assert!(self.global_types.insert(glo, ty).is_none(), "ice: bound a global twice");
  }

  pub fn add_type_info(&mut self, data: Ident, info: DataInfo) {
    assert!(self.data_infos.insert(data, info).is_none(), "ice: added data info twice");
  }

  pub fn get_type_info(&self, data: &Ident) -> DataInfo {
    self.data_infos.get(data).expect("ice: forgot to insert data info for data declaration").clone()
  }

  pub fn bind(&mut self, name: String, ty: Val) {
    self.define(
      Val::Var(ValVar {
        name,
        lvl: self.lvl,
        span: Span::default(),
      }),
      ty,
    );
  }

  pub fn define(&mut self, val: Val, ty: Val) {
    trace!("defined `{}` with type `{}`", val, ty);
    self.env.0.insert(0, val);
    self.types.insert(0, ty);
    self.lvl += 1;
  }
}
