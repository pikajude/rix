#![feature(pattern)]
#![feature(termination_trait_lib)]
#![feature(trait_alias)]
#![feature(try_trait)]
#![feature(assert_matches)]

#[macro_use] extern crate derive_more;
#[macro_use] extern crate lazy_static;
#[macro_use] extern crate serde;
#[macro_use] extern crate thiserror;
#[macro_use] extern crate slog_scope;

#[doc(no_inline)] pub use anyhow::{anyhow, bail, ensure, Context as _, Result};
#[doc(no_inline)] pub use codespan::{FileId, Files, Span};
#[doc(no_inline)] pub use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
pub use cons_list::*;
use error::Catchable;
pub use error::{LocatedError, LocatedStdError, SomeLocatedError};
pub use hash::{Context as HashContext, Encoding, Hash, HashType, Sink as HashSink};
pub use nar::PathFilter;
use parking_lot::Mutex;
pub use pos::*;
pub use rusqlite::{named_params, params, OptionalExtension as _};
pub use sqlite::Sqlite;
use std::{
  fmt::{self, Debug, Display, Formatter},
  ops::Try,
  path::Path,
  process::Termination,
  str::pattern::{Pattern, Searcher},
};

pub mod base32;
mod cons_list;
pub mod error;
pub mod hash;
pub mod logger;
pub mod nar;
pub mod pipe;
mod pos;
pub mod sqlite;

lazy_static! {
  #[doc(hidden)]
  pub static ref FILES: Mutex<Files<String>> = Default::default();
}

pub type Ident = string_cache::DefaultAtom;

pub fn show_diagnostic(diag: &Diagnostic<FileId>) -> Result<()> {
  codespan_reporting::term::emit(
    &mut StandardStream::stderr(ColorChoice::Auto),
    &Default::default(),
    &*FILES.lock(),
    diag,
  )?;
  Ok(())
}

pub fn break_str<'a, P: Pattern<'a>>(s: &'a str, pattern: P) -> Option<(&'a str, &'a str)> {
  let mut search = pattern.into_searcher(s);
  let (start, end) = search.next_match()?;

  Some((&s[..start], &s[end..]))
}

/// A newtype wrapper around [`anyhow::Result`]. The [`Termination`] impl acts
/// the same, except that if the error is [`SomeLocatedError`], it will
/// additionally use codespan_reporting to pretty-print the error message and
/// context to stderr.
pub struct NixResult<T = ()>(pub Result<T>);

pub const fn ok() -> NixResult<()> {
  NixResult(Ok(()))
}

impl Termination for NixResult<()> {
  fn report(self) -> i32 {
    match self.0 {
      Ok(()) => self.0.report(),
      Err(x) => match x.downcast::<Catchable>() {
        Ok(c) => {
          let _ = show_diagnostic(&c.diagnose());
          1
        }
        Err(x) => match x.downcast::<SomeLocatedError>() {
          Ok(located) => {
            let _ = show_diagnostic(&located.0.diagnose());
            1
          }
          Err(e) => <Result<()>>::Err(e).report(),
        },
      },
    }
  }
}

impl<T> Try for NixResult<T> {
  type Error = anyhow::Error;
  type Ok = T;

  fn into_result(self) -> Result<Self::Ok, Self::Error> {
    self.0
  }

  fn from_error(v: Self::Error) -> Self {
    Self(Err(v))
  }

  fn from_ok(v: Self::Ok) -> Self {
    Self(Ok(v))
  }
}
