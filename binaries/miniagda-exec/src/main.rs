use clap::Parser;
use env_logger::Env;

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
  path: std::path::PathBuf,
}

fn main() {
  let args = Args::parse();

  env_logger::Builder::from_env(Env::default().default_filter_or("info")).format_timestamp(None).init();

  match miniagda::elaborate(args.path) {
    Ok(()) => log::info!(target: "miniagda", "all done"),
    Err(e) => log::error!(target: "miniagda", "error: {}", e),
  }
}
