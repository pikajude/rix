use crate::{
  store::{Store, StorePath},
  util::*,
};
use once_cell::sync::OnceCell;
use serde::{de::DeserializeOwned, Serialize};
use std::{path::Path, time::SystemTime};

static CACHE: OnceCell<Cache> = OnceCell::new();

const ADD: &str = "insert or replace into Cache(input, info, path, immutable, timestamp) values \
                   (:input, :info, :path, :immutable, :timestamp)";

const LOOKUP: &str = "select info, path, immutable, timestamp from Cache where input = :input";

pub struct CacheItem<I> {
  pub expired: bool,
  pub info: I,
  pub path: StorePath,
}

pub struct Cache(Sqlite);

impl Cache {
  fn open() -> Result<Self> {
    let cache_dir = dirs::cache_dir().expect("no cache dir").join("rix");
    std::fs::create_dir_all(&cache_dir)?;
    let conn = Sqlite::open(cache_dir.join("fetcher-cache-v1.sqlite"))?;

    conn.lock().execute_batch(
      "
        pragma synchronous = off;
        pragma main.journal_mode = truncate;
        create table if not exists Cache (
            input     text not null,
            info      text not null,
            path      text not null,
            immutable integer not null,
            timestamp integer not null,
            primary key (input)
        );
      ",
    )?;

    Ok(Self(conn))
  }

  pub fn get() -> Result<&'static Self> {
    CACHE.get_or_try_init(Self::open)
  }

  pub fn insert<S: Store + ?Sized, I: Serialize, A: Serialize>(
    &self,
    store: &S,
    input: &I,
    info: &A,
    path: &StorePath,
    immutable: bool,
  ) -> Result<()> {
    let input_json = serde_json::to_string(input)?;
    let info_json = serde_json::to_string(info)?;
    let path = store.print_store_path(path);

    self.0.lock().execute(
      ADD,
      named_params! {
        ":input": input_json,
        ":info": info_json,
        ":path": path,
        ":immutable": immutable,
        ":timestamp": systime()
      },
    )?;

    Ok(())
  }

  pub fn lookup_expired<S: Store + ?Sized, A: Serialize, I: DeserializeOwned>(
    &self,
    store: &S,
    info: &A,
  ) -> Result<Option<CacheItem<I>>> {
    let in_attrs = serde_json::to_string(info)?;

    let conn = self.0.lock();
    let mut stmt = conn.prepare(LOOKUP)?;

    let inf = stmt
      .query_and_then::<_, anyhow::Error, _, _>(named_params! { ":input": in_attrs }, |row| {
        let info_json = row.get::<_, String>(0)?;
        let store_path = store.parse_store_path(Path::new(&row.get::<_, String>(1)?))?;
        let immutable = row.get::<_, bool>(2)?;
        let timestamp = row.get::<_, i64>(3)?;

        if !store.is_valid_path(&store_path)? {
          debug!("ignoring disappeared cache entry");
          return Ok(None);
        }

        debug!(
          "using cache entry `{}' -> `{}', `{}'",
          &in_attrs,
          &info_json,
          store.print_store_path(&store_path)
        );

        Ok(Some(CacheItem {
          expired: !immutable && is_expired(timestamp),
          info: serde_json::from_str(&info_json)?,
          path: store_path,
        }))
      })?
      .next()
      .transpose()?
      .flatten();

    Ok(inf)
  }
}

fn is_expired(timestamp: i64) -> bool {
  timestamp + 3600 < systime()
}

#[inline]
fn systime() -> i64 {
  SystemTime::now()
    .duration_since(SystemTime::UNIX_EPOCH)
    .unwrap()
    .as_secs() as i64
}
