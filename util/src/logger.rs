use std::sync::Mutex;

use anyhow::Result;
use slog::{Drain, FnValue, Record};

pub fn init() -> Result<()> {
  let logger = slog::Logger::root(
    Mutex::new(slog_envlogger::new(slog_term::term_full()).fuse()).fuse(),
    slog::o!("location" => FnValue(move |r: &Record| {
      format!("{}:{}", r.location().file, r.location().line)
    })),
  );

  let log_guard = slog_scope::set_global_logger(logger);
  std::mem::forget(log_guard);

  slog_stdlog::init()?;

  Ok(())
}
