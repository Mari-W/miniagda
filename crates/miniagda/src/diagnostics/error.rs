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
      Error::SurfaceToCore(e) => write!(f, "[SurfaceToCore] {}", e),
      Error::Parse(e) => write!(f, "[Parse] {}", e),
      Error::Lex(e) => write!(f, "[Lex] {}", e),
      Error::Elab(e) => write!(f, "[Elaboration] {}", e),
    }
  }
}

#[derive(Clone, Debug)]
pub enum SurfaceToCoreErr {
  UnboundName { name: String, span: Span },
  GlobalExists { name: String, span: Span },
  MisnamedCls { name: Ident, cls: Ident },
  UnresolvedCstr { name: Ident },
  DuplicatedPatternVariable { name: Ident },
}

impl Display for SurfaceToCoreErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      SurfaceToCoreErr::UnboundName { name, .. } => write!(f, "could not resolve variable {}", name),
      SurfaceToCoreErr::GlobalExists { name, .. } => write!(f, "constructor or data type with name {} already exists", name),
      SurfaceToCoreErr::MisnamedCls { name, cls } => write!(f, "clause with name {} does not begin with function name {} where it is defined on", cls, name),
      SurfaceToCoreErr::UnresolvedCstr { name } => write!(f, "constructor {} was never defined and cannot be matched on", name),
      SurfaceToCoreErr::DuplicatedPatternVariable { name } => write!(f, "found duplicated pattern variable {} in clause", name),
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
      ParseErr::FileNotFound { path } => write!(f, "file {} does not exist", path),
      ParseErr::UnexpectedToken { pos: span, expected } => write!(f, "expected one of {} at position {}", expected, span),
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
  LevelTooHigh { tm: Val, max: usize },
  ExpectedData { expected: Ident, got: Tm },
  ExpectedParam { expected: Ident, got: Option<Tm> },
  ExpectedIndex { expected: Ident, got: Option<Tm> },
  UnexpectedArg { got: Tm },
  TypeMismatch { ty1: Val, ty2: Val, v1: Val, v2: Val },
  FunctionTypeExpected { tm: Tm, got: Val },
  AttemptAbsInfer { tm: Tm },
}

impl Display for ElabErr {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ElabErr::ExpectedSetCtx { got } => write!(f, "expected type of kind Setℓ in context binding, but got `{}`", got),
      ElabErr::ExpectedSetAll { got } => write!(f, "expected type of kind Setℓ in a ∀-binding, but got `{}`", got),
      ElabErr::LevelTooHigh { tm, max } => write!(f, "term `{}` exceeds maximum set level `{}` data type", tm, max),
      ElabErr::ExpectedParam { expected, got: Some(got) } => write!(f, "expected data type parameter `{}`, but got `{}`", expected, got),
      ElabErr::ExpectedParam { expected, got: None } => write!(f, "expected data type parameter `{}`, but got found nothing", expected),
      ElabErr::ExpectedIndex { expected, got: Some(got) } => write!(f, "expected data type index `{}`, but got `{}`", expected, got),
      ElabErr::ExpectedIndex { expected, got: None } => write!(f, "expected data type index `{}`, but got found nothing", expected),
      ElabErr::UnexpectedArg { got } => write!(f, "unexpected data type argument `{}`", got),
      ElabErr::TypeMismatch { ty1, ty2, v1, v2 } => write!(f, "type mismatch between `{}` and `{}`, more specifically `{}` is not `{}`", ty1, ty2, v1, v2),
      ElabErr::FunctionTypeExpected { tm, got } => write!(f, "expected `{}` to be a function type, but got `{}`", tm, got),
      ElabErr::ExpectedData { expected, got } => write!(f, "expected constructor to end in data type  `{}`, but got `{}`", expected, got),
      ElabErr::AttemptAbsInfer { tm } => write!(f, "cannot infer type for abstraction `{}`", tm),
      ElabErr::ExpectedSetData { got } => write!(f, "expected data type definition to end in Setℓ, but got `{}`", got),
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
