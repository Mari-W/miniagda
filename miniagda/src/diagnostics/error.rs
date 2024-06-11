use std::fmt::Display;

use crate::syntax::{
  core::{Tm, Val},
  Ident,
};

use super::span::Span;

// -----------------------------------------------------------------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug)]
pub enum Error {
  Translation(TransErr),
  Parsing(ParseErr),
  Lexing(LexErr),
  Elaboration(ElabErr),
}

#[derive(Clone, Debug)]
pub enum TransErr {
  UnboundVariable { ident: Ident },
  DuplicatedGlobal { ident: Ident },
  FunctionNameExpected { function: Ident, clause: Ident },
  UnresolvedConstructor { ident: Ident },
  DuplicatedPatternVariable { ident: Ident },
}

#[derive(Clone, Debug)]
pub enum ParseErr {
  FileNotFound { path: String },
  UnexpectedToken { pos: usize, expected: String },
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum LexErr {
  #[default]
  UnknownCharacter,
}

#[derive(Clone, Debug)]
pub enum ElabErr {
  ExpectedSetData { got: Tm },
  ExpectedSetCtx { got: Val },
  ExpectedSetAll { got: Val },
  ExpectedSetFun { got: Val },
  LevelTooHigh { tm: Val, max: usize },
  ExpectedData { expected: Ident, got: Tm },
  ExpectedParam { expected: Ident, got: Option<Tm> },
  ExpectedIndex { expected: Ident, got: Option<Tm> },
  UnexpectedArg { got: Tm },
  TypeMismatch { ty1: Val, ty2: Val, v1: Val, v2: Val },
  FunctionTypeExpected { tm: Tm, got: Val },
  AttemptAbsInfer { tm: Tm },
  ExpectedDataForPat { got: Tm },
  CstrNotPresent { data: Ident, got: Ident },
  MisMatchPatAmount { expected: usize, got: usize, ident: Ident, span: Span },
  TooManyPatterns { expected: usize, got: usize, span: Span },
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Trait Impls
// -----------------------------------------------------------------------------------------------------------------------------------

macro_rules! impl_from_diag_enum {
  ($ident:path; $variant:ident) => {
    impl From<$ident> for Error {
      fn from(value: $ident) -> Self {
        Error::$variant(value)
      }
    }
  };
}

impl_from_diag_enum!(TransErr; Translation);
impl_from_diag_enum!(LexErr; Lexing);
impl_from_diag_enum!(ParseErr; Parsing);
impl_from_diag_enum!(ElabErr; Elaboration);

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Error::Translation(e) => write!(f, "{e}"),
      Error::Parsing(e) => write!(f, "{e}"),
      Error::Lexing(e) => write!(f, "{e}"),
      Error::Elaboration(e) => write!(f, "{e}"),
    }
  }
}

impl Display for TransErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      TransErr::UnboundVariable { ident, .. } => write!(f, "could not resolve unbound variable `{ident}`"),
      TransErr::DuplicatedGlobal { ident } => write!(f, "constructor or data type with name `{ident}` already exists"),
      TransErr::FunctionNameExpected { function, clause } => {
        write!(f, "clause with name `{function}` does not begin with function name `{clause}` where it is defined on")
      }
      TransErr::UnresolvedConstructor { ident } => write!(f, "could not resolve constructor `{ident}` was never defined and cannot be matched on"),
      TransErr::DuplicatedPatternVariable { ident } => write!(f, "found duplicated pattern variable `{ident}` in clause"),
    }
  }
}

impl Display for ParseErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ParseErr::FileNotFound { path } => write!(f, "could not find file {path}"),
      ParseErr::UnexpectedToken { pos: span, expected } => write!(f, "expected one of {expected} at position {span}"),
    }
  }
}

impl Display for LexErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LexErr::UnknownCharacter => write!(f, "unknown character"), // TODO: what character,
    }
  }
}

impl Display for ElabErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ElabErr::ExpectedSetCtx { got } => write!(f, "expected type of kind Setℓ in context binding, but got `{got}`"),
      ElabErr::ExpectedSetAll { got } => write!(f, "expected type of kind Setℓ in a ∀-binding, but got `{got}`"),
      ElabErr::LevelTooHigh { tm, max } => write!(f, "term `{tm}` exceeds maximum set level `{max}` data type"),
      ElabErr::ExpectedParam { expected, got: Some(got) } => write!(f, "expected data type parameter `{expected}`, but got `{got}`"),
      ElabErr::ExpectedParam { expected, got: None } => write!(f, "expected data type parameter `{expected}`, but got found nothing"),
      ElabErr::ExpectedIndex { expected, got: Some(got) } => write!(f, "expected data type index `{expected}`, but got `{got}`"),
      ElabErr::ExpectedIndex { expected, got: None } => write!(f, "expected data type index `{expected}`, but got found nothing"),
      ElabErr::UnexpectedArg { got } => write!(f, "unexpected data type argument `{got}`"),
      ElabErr::TypeMismatch { ty1, ty2, v1, v2 } => write!(f, "type mismatch between `{ty1}` and `{ty2}`, more specifically `{v1}` is not `{v2}`"),
      ElabErr::FunctionTypeExpected { tm, got } => write!(f, "expected `{tm}` to be a function type, but got `{got}`"),
      ElabErr::ExpectedData { expected, got } => write!(f, "expected constructor to end in data type  `{expected}`, but got `{got}`"),
      ElabErr::AttemptAbsInfer { tm } => write!(f, "cannot infer type for abstraction `{tm}`"),
      ElabErr::ExpectedSetData { got } => write!(f, "expected data type definition to end in Setℓ, but got `{got}`"),
      ElabErr::ExpectedSetFun { got } => write!(f, "expected function type to be of kind Setℓ for some ℓ, but got `{got}`"),
      ElabErr::ExpectedDataForPat { got } => write!(f, "expected data type to match with constructor pattern on, but got `{got}`"),
      ElabErr::CstrNotPresent { data, got } => write!(f, "data type `{data}` does not have a constructor named `{got}`"),
      ElabErr::MisMatchPatAmount { expected, got, ident, .. } => write!(f, "expected {expected} patterns to match constructor `{ident}`, but got {got}"),
      ElabErr::TooManyPatterns { expected, got, .. } => write!(f, "expected at most {expected} patterns to match clause, but got {got}"),
    }
  }
}
