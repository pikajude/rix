use crate::Result;
use once_cell::sync::OnceCell;
use slog::{Drain, FnValue, Record};
use std::sync::Mutex;

static LOGGER_INIT: OnceCell<()> = OnceCell::new();

fn init_logger() -> Result<()> {
  let logger = slog::Logger::root(
    Mutex::new(slog_envlogger::new(slog_term::term_full()).fuse()).fuse(),
    slog::o!("location" => FnValue(|r: &Record| {
      format!("{}:{}", r.location().file, r.location().line)
    })),
  );

  let log_guard = slog_scope::set_global_logger(logger);
  std::mem::forget(log_guard);

  slog_stdlog::init()?;

  Ok(())
}

pub fn init() -> Result<()> {
  LOGGER_INIT.get_or_try_init(init_logger)?;

  Ok(())
}
