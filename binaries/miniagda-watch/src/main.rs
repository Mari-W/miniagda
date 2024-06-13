#![feature(panic_info_message)]

use clap::Parser;
use futures::StreamExt;
use inotify::{Inotify, WatchMask};
use miniagda::diagnostics::error::Error;
use std::io::Write;
use std::path::Path;
use std::{io, panic};
use termion::cursor;
use termion::{clear, color};

#[derive(Parser, Debug)]
#[command(author, version, about)]
struct Args {
  path: std::path::PathBuf,
}

fn check<P: AsRef<Path> + panic::UnwindSafe>(path: P) {
  print!("{}{}", clear::All, cursor::Goto(2, 1));
  match panic::catch_unwind(|| miniagda::elaborate(path)) {
    Ok(Ok(())) => print!("{}✓{} all done", color::Fg(color::Green), color::Fg(color::Reset)),
    Ok(Err(e)) => {
      let e = match e {
        Error::Translation(e) => format!("{e}"),
        Error::Parsing(e) => format!("{e}"),
        Error::Lexing(e) => format!("{e}"),
        Error::Elaboration(e) => format!("{e}"),
      };
      print!("{}⨯{} {}", color::Fg(color::Red), color::Fg(color::Reset), e);
    }
    Err(_) => {}
  }
  io::stdout().flush().unwrap();
}

#[tokio::main]
async fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
  panic::set_hook(Box::new(|info| {
    print!(
      "{}⨯{} miniagda crashed: {}",
      color::Fg(color::Red),
      color::Fg(color::Reset),
      info
    );
  }));
  let args = Args::parse();
  let inotify = Inotify::init().expect("error initializing inotify");
  inotify.watches().add(&args.path, WatchMask::MODIFY).expect("failed to add file watch");
  let mut buffer = [0; 1024];
  let mut stream = inotify.into_event_stream(&mut buffer)?;
  check(&args.path);
  while let Some(event_or_error) = stream.next().await {
    let _ = event_or_error?;
    check(&args.path);
  }
  Ok(())
}
