use std::fmt::Display;

use crate::syntax::{
  core::{Tm, Val},
  Ident,
};

use super::span::Span;

#[derive(Clone, Debug)]
pub enum Error {
  SurfaceToCore(SurfaceToCoreErr),
  Parse(ParseErr),
  Lex(LexErr),
  Elab(ElabErr),
}

impl Display for Error {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Error::SurfaceToCore(e) => write!(f, "[SurfaceToCore] {e}"),
      Error::Parse(e) => write!(f, "[Parse] {e}"),
      Error::Lex(e) => write!(f, "[Lex] {e}"),
      Error::Elab(e) => write!(f, "[Elaboration] {e}"),
    }
  }
}

#[derive(Clone, Debug)]
pub enum SurfaceToCoreErr {
  UnboundName { name: String, span: Span },
  GlobalExists { name: String, span: Span },
  MisnamedCls { name: String, cls: Ident },
  UnresolvedCstr { name: Ident },
  DuplicatedPatternVariable { name: Ident },
}

impl Display for SurfaceToCoreErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SurfaceToCoreErr::UnboundName { name, .. } => write!(f, "could not resolve variable {name}"),
      SurfaceToCoreErr::GlobalExists { name, .. } => write!(f, "constructor or data type with name {name} already exists"),
      SurfaceToCoreErr::MisnamedCls { name, cls } => write!(f, "clause with name {cls} does not begin with function name {name} where it is defined on"),
      SurfaceToCoreErr::UnresolvedCstr { name } => write!(f, "constructor {name} was never defined and cannot be matched on"),
      SurfaceToCoreErr::DuplicatedPatternVariable { name } => write!(f, "found duplicated pattern variable {name} in clause"),
    }
  }
}

#[derive(Clone, Debug)]
pub enum ParseErr {
  FileNotFound { path: String },
  UnexpectedToken { pos: usize, expected: String },
}

impl Display for ParseErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ParseErr::FileNotFound { path } => write!(f, "file {path} does not exist"),
      ParseErr::UnexpectedToken { pos: span, expected } => write!(f, "expected one of {expected} at position {span}"),
    }
  }
}

#[derive(Clone, Debug, PartialEq, Default)]
pub enum LexErr {
  #[default]
  UnknownCharacter,
}

impl Display for LexErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      LexErr::UnknownCharacter => write!(f, "unknown character"), // TODO: what character,
    }
  }
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
      ElabErr::TooManyPatterns { expected, got, .. } =>  write!(f, "expected at most {expected} patterns to match clause, but got {got}"),
    }
  }
}

macro_rules! impl_from_diag_enum {
  ($ident:path; $variant:ident) => {
    impl From<$ident> for Error {
      fn from(value: $ident) -> Self {
        Error::$variant(value)
      }
    }
  };
}

impl_from_diag_enum!(SurfaceToCoreErr; SurfaceToCore);
impl_from_diag_enum!(LexErr; Lex);
impl_from_diag_enum!(ParseErr; Parse);
impl_from_diag_enum!(ElabErr; Elab);
