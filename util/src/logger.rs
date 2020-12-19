use once_cell::sync::Lazy;
use slog::{Drain, FnValue, Record};
use std::sync::Mutex;

static LOGGER_INIT: Lazy<()> = Lazy::new(|| {
  let logger = slog::Logger::root(
    Mutex::new(slog_envlogger::new(slog_term::term_full()).fuse()).fuse(),
    slog::o!("location" => FnValue(move |r: &Record| {
      format!("{}:{}", r.location().file, r.location().line)
    })),
  );

  let log_guard = slog_scope::set_global_logger(logger);
  std::mem::forget(log_guard);

  slog_stdlog::init().expect("unable to initialize slog-stdlog");
});

pub fn init() {
  Lazy::force(&LOGGER_INIT);
}
