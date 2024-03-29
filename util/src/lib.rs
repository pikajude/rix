#![feature(
  try_trait_v2,
  termination_trait_lib,
  pattern,
  assert_matches,
  trait_alias
)]

pub extern crate rusqlite;

pub use self::cons_list::*;
pub use self::error::{LocatedError, LocatedStdError, SomeLocatedError};
pub use self::hash::{Context as HashContext, Encoding, Hash, HashType, Sink as HashSink};
pub use self::nar::PathFilter;
pub use self::pos::*;
pub use anyhow::{anyhow, bail, ensure, Context as _, Result};
pub use codespan::{FileId, Files, Span};
pub use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::term::termcolor::{ColorChoice, StandardStream};
pub use derive_more::{Deref, DerefMut, Display};
pub use enum_as_inner::EnumAsInner;
pub use lazy_static::lazy_static;
pub use rusqlite::{named_params, params, OptionalExtension as _};
pub use serde::{Deserialize, Serialize};
pub use slog_scope::{debug, error, info, trace, warn};
pub use sqlite::Sqlite;
pub use thiserror::Error;

use self::error::Catchable;
use nix::fcntl::OFlag;
use nix::unistd::pipe2;
use parking_lot::Mutex;
use std::convert::Infallible;
use std::fmt::{self, Debug, Display, Formatter};
use std::fs::File;
use std::ops::{ControlFlow, FromResidual, Try};
use std::os::unix::prelude::FromRawFd;
use std::path::{Path, PathBuf};
use std::process::Termination;
use std::str::pattern::{Pattern, Searcher};
use std::sync::atomic::{AtomicBool, Ordering};

pub mod base32;
mod cons_list;
pub mod error;
pub mod hash;
pub mod logger;
pub mod nar;
mod pos;
pub mod sqlite;

lazy_static! {
  #[doc(hidden)]
  pub static ref FILES: Mutex<Files<String>> = Default::default();
}

impl FILES {
  #[allow(dead_code)]
  pub(crate) fn dump_pos(p: Pos) -> Result<()> {
    let f = FILES.lock();
    let contents = f.source_slice(p.0, p.1)?;
    debug!("dump_pos: {}", contents);
    Ok(())
  }
}

#[doc(hidden)]
pub mod ident_gen {
  #![allow(clippy::all)]
  include!(concat!(env!("OUT_DIR"), "/ident_gen.rs"));
}

pub use ident_gen::Ident;

pub fn show_diagnostic(diag: &Diagnostic<FileId>) -> Result<()> {
  codespan_reporting::term::emit(
    &mut StandardStream::stderr(ColorChoice::Auto),
    &Default::default(),
    &*FILES.lock(),
    diag,
  )?;
  Ok(())
}

pub fn pipe() -> Result<(impl std::io::Read, impl std::io::Write)> {
  let (read, write) = pipe2(OFlag::O_CLOEXEC)?;
  Ok(unsafe { (File::from_raw_fd(read), File::from_raw_fd(write)) })
}

static INTERRUPTED: AtomicBool = AtomicBool::new(false);

pub fn interrupted() -> bool {
  INTERRUPTED.load(Ordering::Acquire)
}

pub fn init_rix() -> Result<()> {
  self::logger::init()?;

  ctrlc::set_handler(|| {
    INTERRUPTED.store(true, Ordering::Release);
    eprintln!("received interrupt!");
  })?;

  Ok(())
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
  type Output = T;
  type Residual = Result<Infallible>;

  fn from_output(output: Self::Output) -> Self {
    Self(Ok(output))
  }

  fn branch(self) -> ControlFlow<Self::Residual, Self::Output> {
    match self.0 {
      Ok(v) => ControlFlow::Continue(v),
      Err(e) => ControlFlow::Break(Err(e)),
    }
  }
}

impl<T, E> FromResidual<Result<Infallible, E>> for NixResult<T>
where
  anyhow::Error: From<E>,
{
  fn from_residual(residual: Result<Infallible, E>) -> Self {
    match residual {
      Err(e) => Self(Err(anyhow::Error::from(e))),
      Ok(_) => unreachable!(),
    }
  }
}

pub trait PathExt {
  fn append<P: AsRef<Path>>(&self, other: P) -> PathBuf;
}

impl PathExt for Path {
  fn append<P: AsRef<Path>>(&self, other: P) -> PathBuf {
    let other = other.as_ref();
    self.join(other.strip_prefix("/").unwrap_or(other))
  }
}

pub trait StrExt {
  fn break_on<'a, P: Pattern<'a>>(&'a self, pattern: P) -> Option<(&'a Self, &'a Self)>;
}

impl StrExt for str {
  fn break_on<'a, P: Pattern<'a>>(&'a self, pattern: P) -> Option<(&'a Self, &'a Self)> {
    let mut search = pattern.into_searcher(self);
    let (start, end) = search.next_match()?;

    Some((&self[..start], &self[end..]))
  }
}

pub trait SliceExt<T> {
  fn take(&self, n: usize) -> &Self;
  fn take_mut(&mut self, n: usize) -> &mut Self;
}

impl<T> SliceExt<T> for [T] {
  fn take(&self, n: usize) -> &Self {
    &self[0..std::cmp::min(n, self.len())]
  }

  fn take_mut(&mut self, n: usize) -> &mut Self {
    let l = self.len();
    &mut self[0..std::cmp::min(n, l)]
  }
}
