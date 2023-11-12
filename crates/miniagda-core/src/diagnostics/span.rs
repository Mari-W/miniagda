use crate::syntax::{core, surface};

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
  pub file: String,
  pub start: usize,
  pub end: usize,
}

impl Span {
  pub fn dummy() -> Self {
    Span {
      file: "".to_owned(),
      start: 0,
      end: 0,
    }
  }
}

pub trait Spanned {
  fn span(&self) -> Span;
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

impl_spanned_struct!(surface::Ident);
impl_spanned_struct!(surface::TmApp);
impl_spanned_struct!(surface::TmAbs);
impl_spanned_struct!(surface::TmAll);
impl_spanned_struct!(surface::TmSet);
impl_spanned_enum!(surface::Tm; Var, App, Abs, All, Set, Brc);
impl_spanned_struct!(surface::Ctx);
impl_spanned_struct!(surface::Cstr);
impl_spanned_struct!(surface::Data);
impl_spanned_enum!(surface::Decl; Data);
impl_spanned_struct!(surface::Prog);

impl_spanned_struct!(core::Ident);
impl_spanned_struct!(core::TmVar);
impl_spanned_struct!(core::TmApp);
impl_spanned_struct!(core::TmAbs);
impl_spanned_struct!(core::TmAll);
impl_spanned_struct!(core::TmSet);
impl_spanned_enum!(core::Tm; Var, Glo, App, Abs, All, Set);
impl_spanned_struct!(core::Ctx);
impl_spanned_struct!(core::Tel);
impl_spanned_struct!(core::Cstr);
impl_spanned_struct!(core::Data);
impl_spanned_enum!(core::Decl; Data);
impl_spanned_struct!(core::Prog);