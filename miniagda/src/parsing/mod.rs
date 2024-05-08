pub mod lex;
pub mod parse;

use crate::{
  diagnostics::error::{Error, ParseErr},
  diagnostics::Result,
  parsing::lex::Token,
  syntax::surface::Prog,
};
use std::{fs::read_to_string, path::Path};

pub fn parse<P: AsRef<Path>>(path: P) -> Result<Prog> {
  let file_path = path.as_ref().to_string_lossy().to_string();
  let src = read_to_string(path).map_err(|_| Error::from(ParseErr::FileNotFound { path: file_path.clone() }))?;
  let toks = lex::lex(src.as_str()).map_err(Error::from)?;
  let indented = lex::process_indent(toks, |t| *t == Token::Where || *t == Token::Equals, |t| *t == Token::NewLine);
  // println!("{:?}", indented);
  let parsed = parse::parser::prog(&indented, &file_path).map_err(|e| ParseErr::UnexpectedToken {
    pos: e.location,
    expected: e.expected.to_string(),
  })?;
  // println!("{parsed}");
  Ok(parsed)
}
