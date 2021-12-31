use super::*;
use crossbeam::atomic::AtomicCell;
use serde::de::{Deserialize, Error, SeqAccess, Visitor};
use serde::ser::{Serialize, SerializeSeq};

#[derive(Clone, serde::Serialize, serde::Deserialize)]
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

  pub fn nowhere(v: T) -> Self {
    Self {
      pos: Pos::none(),
      v,
    }
  }
}

#[derive(Ord, PartialOrd, Eq, PartialEq, Clone, Copy, Hash)]
pub struct Pos(pub FileId, pub Span);

// safety: the FILES global is always initialized with one (empty) file, so file
// ID 1 is always valid
const NO_FILE: FileId = unsafe { std::mem::transmute(1) };

pub static CURRENT_FILE_ID: AtomicCell<FileId> = AtomicCell::new(NO_FILE);

impl Pos {
  pub fn none() -> Self {
    Self(NO_FILE, Span::initial())
  }
}

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
      Path::new(filename).file_name().map_or_else(
        || String::from("<unknown>"),
        |p| p.to_string_lossy().to_string()
      ),
      loc1.line.number(),
      loc1.column.number(),
      loc2.line.number(),
      loc2.column.number()
    )
  }
}

impl Serialize for Pos {
  fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
  where
    S: serde::Serializer,
  {
    let mut seq = serializer.serialize_seq(Some(2))?;
    seq.serialize_element(&self.1.start().to_usize())?;
    seq.serialize_element(&self.1.end().to_usize())?;
    seq.end()
  }
}

impl<'de> Deserialize<'de> for Pos {
  fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
  where
    D: serde::Deserializer<'de>,
  {
    deserializer.deserialize_seq(PosVisitor)
  }
}

struct PosVisitor;

impl<'de> Visitor<'de> for PosVisitor {
  type Value = Pos;

  fn expecting(&self, formatter: &mut Formatter) -> fmt::Result {
    write!(formatter, "a pair of usizes")
  }

  fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
  where
    A: SeqAccess<'de>,
  {
    let start = seq
      .next_element::<usize>()?
      .ok_or_else(|| A::Error::custom("expected start item"))?;
    let end = seq
      .next_element::<usize>()?
      .ok_or_else(|| A::Error::custom("expected end item"))?;
    Ok(Pos(
      CURRENT_FILE_ID.load(),
      Span::new(start as u32, end as u32),
    ))
  }
}

impl<T: Debug> Debug for Located<T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    self.v.fmt(f)
  }
}
