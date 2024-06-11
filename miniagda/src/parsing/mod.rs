mod lex;
mod parse;

use crate::{
  diagnostics::error::{Error, ParseErr},
  diagnostics::Result,
  syntax::surf::Prog,
};
use std::{fs, path::Path};

// -----------------------------------------------------------------------------------------------------------------------------------
// Public API
// -----------------------------------------------------------------------------------------------------------------------------------

pub fn parse(path: impl AsRef<Path>) -> Result<Prog> {
  let path_as_string = path.as_ref().to_string_lossy().to_string();
  let file_content = fs::read_to_string(path).map_err(|_| Error::from(ParseErr::FileNotFound { path: path_as_string.clone() }))?;
  let tokens = lex::lex(file_content.as_str()).map_err(Error::from)?;
  let ast = parse::parse(&tokens, &path_as_string).map_err(Error::from)?;
  Ok(ast)
}
