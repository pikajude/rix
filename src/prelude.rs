pub use crate::{
  base32,
  hash::{self, Hash},
  path::{Hash as StorePathHash, Path as StorePath},
};
pub use anyhow::{Context as _, Result};
use std::str::pattern::{Pattern, Searcher};

pub fn break_str<'a, P: Pattern<'a>>(s: &'a str, pattern: P) -> Option<(&'a str, &'a str)> {
  let mut search = pattern.into_searcher(s);
  let (start, end) = search.next_match()?;

  Some((&s[..start], &s[end..]))
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum FileIngestionMethod {
  Flat,
  Recursive,
}

impl FileIngestionMethod {
  pub fn prefix(&self) -> &'static str {
    match self {
      Self::Flat => "",
      Self::Recursive => "r:",
    }
  }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum Repair {
  Off,
  On,
}
