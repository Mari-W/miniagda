use std::fmt::Display;

use crate::diagnostics::span::Span;

use super::Ident;

// -----------------------------------------------------------------------------------------------------------------------------------
// Terms

#[derive(Clone, Debug)]
pub struct TmApp {
  pub left: Box<Tm>,
  pub right: Box<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TmAbs {
  pub ident: Ident,
  pub ty: Box<Tm>,
  pub body: Box<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TmAll {
  pub ident: Ident,
  pub dom: Box<Tm>,
  pub codom: Box<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct TmSet {
  pub level: usize,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Tm {
  Var(Ident),
  App(TmApp),
  Abs(TmAbs),
  All(TmAll),
  Set(TmSet),
  Brc(Box<Tm>),
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Contexts

#[derive(Clone, Debug)]
pub struct Ctx {
  pub binds: Vec<(Ident, Tm)>,
  pub span: Span,
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Data Types

#[derive(Clone, Debug)]
pub struct Cstr {
  pub ident: Ident,
  pub args: Ctx,
  pub params: Vec<Tm>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct Data {
  pub ident: Ident,
  pub params: Ctx,
  pub indices: Ctx,
  pub set: Tm,
  pub cstrs: Vec<Cstr>,
  pub span: Span,
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Functions

#[derive(Clone, Debug)]
pub struct PatCst {
  pub cstr: Ident,
  pub pats: Vec<Pat>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct PatDot {
  pub tm: Tm,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Pat {
  Var(Ident),
  Cst(PatCst),
  Dot(PatDot),
  Abs(Span),
  Brc(Box<Pat>),
}

#[derive(Clone, Debug)]
pub struct ClsClause {
  pub func: Ident,
  pub pats: Vec<Pat>,
  pub rhs: Tm,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub struct ClsAbsurd {
  pub func: Ident,
  pub pats: Vec<Pat>,
  pub span: Span,
}

#[derive(Clone, Debug)]
pub enum Cls {
  Cls(ClsClause),
  Abs(ClsAbsurd),
}

#[derive(Clone, Debug)]
pub struct Func {
  pub ident: Ident,
  pub ty: Tm,
  pub cls: Vec<Cls>,
  pub span: Span,
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Programs

#[derive(Clone, Debug)]
pub enum Decl {
  Data(Data),
  Func(Func),
}

#[derive(Clone, Debug)]
pub struct Prog {
  pub decls: Vec<Decl>,
  pub span: Span,
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Trait Impls

impl Display for Tm {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Tm::Var(x) => write!(f, "{}", x.name),
      Tm::App(TmApp { left, right, .. }) => write!(f, "{left} {right}"),
      Tm::Abs(TmAbs { ident, ty, body, .. }) => write!(f, "λ ({ident} : {ty}) → {body}"),
      Tm::All(TmAll { ident, dom, codom, .. }) => write!(f, "∀ ({ident} : {dom}) → {codom}"),
      Tm::Set(TmSet { level, .. }) => {
        write!(f, "Set{}", if *level == 0 { String::new() } else { level.to_string() })
      }
      Tm::Brc(tm) => write!(f, "({tm})"),
    }
  }
}

impl Display for Ctx {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      self.binds.iter().map(|(x, tm)| format!("({} : {})", x.name, tm)).collect::<Vec<String>>().join(" ")
    )
  }
}

impl Display for Pat {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Pat::Var(ident) => write!(f, "{ident}"),
      Pat::Cst(PatCst { cstr, pats, .. }) => write!(f, "{} {}", cstr, pats.iter().map(|pat| format!("{pat}")).collect::<Vec<String>>().join(" ")),
      Pat::Dot(PatDot { tm, .. }) => write!(f, ".{tm}"),
      Pat::Abs(_) => write!(f, "()"),
      Pat::Brc(pat) => write!(f, "({pat})"),
    }
  }
}

impl Display for ClsAbsurd {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "{} {}", self.func, self.pats.iter().map(|pat| format!("{pat}")).collect::<Vec<String>>().join(" "),)
  }
}

impl Display for ClsClause {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{} {} = {}",
      self.func,
      self.pats.iter().map(|pat| format!("{pat}")).collect::<Vec<String>>().join(" "),
      self.rhs
    )
  }
}

impl Display for Cls {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Cls::Cls(cls) => write!(f, "{cls}"),
      Cls::Abs(abs) => write!(f, "{abs}"),
    }
  }
}

impl Display for Func {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{} : {}\n{}",
      self.ident,
      self.ty,
      self.cls.iter().map(|cls| format!("{cls}")).collect::<Vec<String>>().join("\n")
    )
  }
}

impl Display for Cstr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{} : {}{}{}",
      self.ident,
      self.args,
      if self.args.binds.is_empty() { " " } else { " → " },
      self.params.iter().map(|tm| format!("{tm}")).collect::<Vec<String>>().join(" ")
    )
  }
}

impl Display for Data {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "data {}{}{} : {}{}{} where\n{}",
      self.ident,
      if self.params.binds.is_empty() { "" } else { " " },
      self.params,
      self.indices,
      if self.indices.binds.is_empty() { "" } else { " → " },
      self.set,
      self.cstrs.iter().map(|cstr| format!("  {cstr}")).collect::<Vec<String>>().join("\n")
    )
  }
}

impl Display for Decl {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Decl::Data(data) => write!(f, "{data}"),
      Decl::Func(func) => write!(f, "{func}"),
    }
  }
}

impl Display for Prog {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "{}",
      self.decls.iter().map(|decl| format!("{decl}")).collect::<Vec<String>>().join("\n\n"),
      // self.ty,
      // self.tm
    )
  }
}
