use self::error::Error;

pub mod error;
pub mod span;

pub type Result<T> = std::result::Result<T, Error>;

#[macro_export]
macro_rules! warn {
  ($($arg:tt)+) => (log::warn!(target: format!("{}:{}", file!(), line!()).as_str(), $($arg)+));
}

#[macro_export]
macro_rules! info {
  ($($arg:tt)+) => (log::info!(target: format!("{}:{}", file!(), line!()).as_str(), $($arg)+));
}

#[macro_export]
macro_rules! debug {
  ($($arg:tt)+) => (log::debug!(target: format!("{}:{}", file!(), line!()).as_str(), $($arg)+));
}

#[macro_export]
macro_rules! trace {
  ($($arg:tt)+) => (log::trace!(target: format!("{}:{}", file!(), line!()).as_str(), $($arg)+));
}
