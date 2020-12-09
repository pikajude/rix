use std::fmt::{self, Debug, Display, Formatter};

mod generated {
  #![allow(clippy::all)]
  include!(concat!(env!("OUT_DIR"), "/parse.rs"));
}

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
pub struct Pos(pub codespan::FileId, pub codespan::Span);

impl Debug for Pos {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "Pos({})", self)
  }
}

impl Display for Pos {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    let files = super::FILES.lock();
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
    Self(unsafe { std::mem::transmute(1) }, codespan::Span::initial())
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
