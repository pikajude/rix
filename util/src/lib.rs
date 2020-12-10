#[macro_use] extern crate lazy_static;
#[macro_use] extern crate thiserror;

pub use anyhow::{Context as _, Result};
pub use codespan::{FileId, Files, Span};
pub use codespan_reporting::diagnostic::{Diagnostic, Label};
pub use cons_list::*;
pub use error::{LocatedError, LocatedStdError, SomeLocatedError};
use parking_lot::Mutex;
use std::fmt::{self, Debug, Display, Formatter};

mod cons_list;
pub mod error;

lazy_static! {
  #[doc(hidden)]
  pub static ref FILES: Mutex<Files<String>> = Default::default();
}

pub type Ident = string_cache::DefaultAtom;

#[derive(Clone)]
pub struct Located<T> {
  pub pos: Pos,
  pub v: T,
}

impl<T> Located<T> {
  pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Located<U> {
    Located {
      pos: self.pos,
      v: f(self.v),
    }
  }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Copy, Hash)]
pub struct Pos(pub FileId, pub Span);

impl Debug for Pos {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "Pos({})", self)
  }
}

impl Display for Pos {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let files = FILES.lock();
    let filename = files.name(self.0);
    let loc1 = files.location(self.0, self.1.start()).unwrap();
    let loc2 = files.location(self.0, self.1.end()).unwrap();
    write!(
      f,
      "{}:{}:{}-{}:{}",
      filename.to_string_lossy(),
      loc1.line.number(),
      loc1.column.number(),
      loc2.line.number(),
      loc2.column.number()
    )
  }
}

impl Pos {
  pub fn none() -> Self {
    Self(unsafe { std::mem::transmute(1) }, Span::initial())
  }
}

pub fn not_located<T>(value: T) -> Located<T> {
  Located {
    pos: Pos::none(),
    v: value,
  }
}

impl<T: Debug> Debug for Located<T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.v.fmt(f)
  }
}
