use crate::*;
use parking_lot::Mutex;
use rusqlite::Connection;
use slog::{Drain, Level};

#[derive(Deref)]
pub struct Sqlite(Mutex<Connection>);

impl Sqlite {
  pub fn open<P: AsRef<Path>>(path: P) -> Result<Self> {
    let mut conn = Connection::open(path)?;

    if slog_scope::logger().is_enabled(Level::Trace) {
      conn.trace(Some(|stmt| trace!("sqlite: {}", stmt.trim_start())));
    }

    Ok(Self(Mutex::new(conn)))
  }
}
