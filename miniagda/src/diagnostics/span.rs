use crate::syntax::{core, surf, Ident};

// -----------------------------------------------------------------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Default)]
pub struct Span {
  pub file: String,
  pub start: usize,
  pub end: usize,
}

pub trait Spanned {
  fn span(&self) -> Span;
}

// -----------------------------------------------------------------------------------------------------------------------------------
// Trait Impls
// -----------------------------------------------------------------------------------------------------------------------------------

impl Spanned for Span {
  fn span(&self) -> Span {
    self.clone()
  }
}

macro_rules! impl_spanned_struct {
  ($name:path) => {
    impl Spanned for $name {
      fn span(&self) -> Span {
        self.span.clone()
      }
    }
  };
}

macro_rules! impl_spanned_enum {
  ($name:path; $($variant:ident),*) => {
      impl Spanned for $name {
        fn span(&self) -> Span {
          match self {
            $(
              Self::$variant(x) => x.span()
            ),*
          }
        }
      }
  };
}

impl_spanned_struct!(Ident);

impl_spanned_struct!(surf::TmApp);
impl_spanned_struct!(surf::TmAbs);
impl_spanned_struct!(surf::TmAll);
impl_spanned_struct!(surf::TmSet);
impl_spanned_enum!(surf::Tm; Var, App, Abs, All, Set, Brc);
impl_spanned_struct!(surf::Ctx);
impl_spanned_struct!(surf::PatId);
impl_spanned_struct!(surf::PatDot);
impl_spanned_enum!(surf::Pat; Id, Dot);
impl_spanned_struct!(surf::ClsClause);
impl_spanned_struct!(surf::ClsAbsurd);
impl_spanned_enum!(surf::Cls; Cls, Abs);
impl_spanned_struct!(surf::Func);
impl_spanned_struct!(surf::Cstr);
impl_spanned_struct!(surf::Data);
impl_spanned_enum!(surf::Decl; Data, Func);
impl_spanned_struct!(surf::Prog);

impl_spanned_struct!(core::TmVar);
impl_spanned_struct!(core::TmApp);
impl_spanned_struct!(core::TmAbs);
impl_spanned_struct!(core::TmAll);
impl_spanned_struct!(core::Set);
impl_spanned_enum!(core::Tm; Var, Data, Cstr, Func, App, Abs, All, Set);
impl_spanned_struct!(core::Ctx);
impl_spanned_struct!(core::Tel);
impl_spanned_struct!(core::PatVar);
impl_spanned_struct!(core::PatCst);
impl_spanned_struct!(core::PatDot);
impl_spanned_enum!(core::Pat; Var, Cst, Dot);
impl_spanned_struct!(core::ClsClause);
impl_spanned_struct!(core::ClsAbsurd);
impl_spanned_enum!(core::Cls; Cls, Abs);
impl_spanned_struct!(core::Func);
impl_spanned_struct!(core::Cstr);
impl_spanned_struct!(core::Data);
impl_spanned_enum!(core::Decl; Data, Func);
impl_spanned_struct!(core::Prog);

impl_spanned_struct!(core::ValVar);
impl_spanned_struct!(core::ValApp);
impl_spanned_struct!(core::ValAbs);
impl_spanned_struct!(core::ValAll);
impl_spanned_enum!(core::Val; Var, Data, Cstr, Func, App, Abs, All, Set);
