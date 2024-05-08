use std::path::Path;

use clap::Parser;
use env_logger::Env;
use miniagda::diagnostics::error::Error;
use miniagda::diagnostics::Result;
use miniagda::{elaboration::elab, parsing::parse, syntax::surface_to_core};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
  path: std::path::PathBuf,
}

fn run<P: AsRef<Path>>(path: P) -> Result<()> {
  let prog = parse(path)?;
  log::trace!(target: "surface", "\n{}", prog);
  let prog = surface_to_core(prog)?;
  log::trace!(target: "core", "\n{}", prog);
  elab(prog)
}

fn main() {
  let args = Args::parse();
  env_logger::Builder::from_env(Env::default().default_filter_or("info")).format_timestamp(None).init();
  match run(args.path) {
    Ok(()) => log::info!(target: "miniagda", "type check successful"),
    Err(e) => match e {
      Error::SurfaceToCore(e) => log::error!(target: "scoping", "{}", e),
      Error::Parse(e) => log::error!(target: "parsing", "{}", e),
      Error::Lex(e) => log::error!(target: "lexing", "{}", e),
      Error::Elab(e) => log::error!(target: "elaboration", "{}", e),
    },
  }
}
