use super::Pos;
use codespan::FileId;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use std::error::Error;
use thiserror::Error;

pub trait LocatedError: Error + Sync + Send {
  fn erased(self) -> anyhow::Error
  where
    Self: Sized + 'static,
  {
    SomeLocatedError(Box::new(self)).into()
  }

  fn diagnose(&self) -> Diagnostic<FileId>;
}

#[derive(Debug, Error)]
pub enum Catchable {
  #[error("assertion failed")]
  Assert(Pos),
  #[error("evaluation aborted with message: `{}'", _1)]
  Throw(Pos, String),
}

impl LocatedError for Catchable {
  fn diagnose(&self) -> Diagnostic<FileId> {
    let pos = match self {
      Self::Assert(p) => p,
      Self::Throw(p, _) => p,
    };
    Diagnostic::error()
      .with_labels(vec![Label::primary(pos.0, pos.1)])
      .with_message(self.to_string())
  }
}

#[derive(Error, Debug)]
#[error("{0}")]
pub struct SomeLocatedError(pub Box<dyn LocatedError>);

#[macro_export]
macro_rules! throw {
  ($pos:expr, $e:expr) => {
    return Err($crate::err!($pos, $e))
  };
  ($pos:expr, $l:literal, $($t:tt)+) => {
    $crate::throw!($pos, format!($l, $($t)+))
  }
}

#[macro_export]
macro_rules! err {
  ($pos:expr, $e:expr) => {
    $crate::LocatedStdError {
      pos: $pos,
      err: $e.into(),
    }
    .erased()
  };

  ($pos:expr, $l:literal, $($t:tt)+) => {
    $crate::err!($pos, format!($l, $($t)+))
  }
}

#[macro_export]
macro_rules! unwrap {
  ($l:expr, $pos:expr, $e:expr) => {
    $l.ok_or_else(|| $crate::err!($pos, $e))?
  };

  ($l:expr, $pos:expr, $msg:literal, $($t:tt)+) => {
    $crate::unwrap!($l, $pos, format!($msg, $($t)+))
  }
}

#[derive(Error, Debug)]
#[error("{err}")]
pub struct LocatedStdError {
  pub pos: Pos,
  pub err: Box<dyn std::error::Error + Send + Sync>,
}

impl LocatedError for LocatedStdError {
  fn diagnose(&self) -> Diagnostic<FileId> {
    Diagnostic::error()
      .with_labels(vec![Label::primary(self.pos.0, self.pos.1)])
      .with_message(self.to_string())
  }
}
