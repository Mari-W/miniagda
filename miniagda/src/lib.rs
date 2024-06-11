#![feature(box_patterns)]
#![allow(clippy::result_large_err)]

pub mod diagnostics;
pub mod elaboration;
pub mod parsing;
pub mod syntax;

pub fn elaborate(path: impl AsRef<std::path::Path>) -> diagnostics::Result<()> {
  elaboration::elaborate(syntax::translate(parsing::parse(path)?)?)
}
