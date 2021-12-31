use super::prelude::*;
use std::cmp::Ordering;
use std::convert::TryInto;
use std::fmt::{self, Debug, Display};
use std::ops::Deref;
use std::str::FromStr;

const HASH_BYTES: usize = 20;
const HASH_CHARS: usize = 32;

lazy_static! {
  pub static ref DUMMY: Path = Path {
    hash: Hash([0xffu8; HASH_BYTES]),
    name: Name(String::from("x"))
  };
}

#[derive(Clone, Ord, PartialOrd, Eq, PartialEq, Debug, Display, Hash)]
#[display(fmt = "{}-{}", hash, name)]
pub struct Path {
  hash: Hash,
  name: Name,
}

impl Path {
  pub fn from_base_name<S: AsRef<str>>(base_name: S) -> Result<Self> {
    let base_name = base_name.as_ref();
    ensure!(
      !(base_name.len() < HASH_CHARS + 1 || base_name.as_bytes()[HASH_CHARS] != b'-'),
      "invalid filename for store path: `{}'",
      base_name
    );

    Ok(Self {
      hash: base_name[..HASH_CHARS].parse()?,
      name: base_name[HASH_CHARS + 1..].parse()?,
    })
  }

  pub(crate) fn from_parts(bytes: &[u8], name: &str) -> Result<Self> {
    Ok(Self {
      hash: Hash(bytes.try_into()?),
      name: name.parse()?,
    })
  }

  pub fn is_derivation(&self) -> bool {
    self.name.ends_with(".drv")
  }

  pub fn derivation_name(&self) -> Result<&str> {
    self
      .name
      .strip_suffix(".drv")
      .ok_or_else(|| anyhow!("store path does not refer to a derivation"))
  }

  pub fn name(&self) -> &str {
    &self.name.0
  }

  pub fn hash_part(&self) -> String {
    self.hash.to_string()
  }
}

#[derive(Copy, Clone, Hash, Eq, PartialEq)]
pub struct Hash([u8; HASH_BYTES]);

impl Hash {
  pub fn decode<S: AsRef<str>>(s: S) -> Result<Self> {
    let s = s.as_ref();
    ensure!(s.len() == HASH_CHARS, "invalid store path hash");
    let v = base32::decode(s.as_bytes())?;
    let mut bytes = [0u8; HASH_BYTES];
    bytes.copy_from_slice(&v[..HASH_BYTES]);
    Ok(Self(bytes))
  }
}

impl FromStr for Hash {
  type Err = anyhow::Error;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Self::decode(s)
  }
}

impl Display for Hash {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut buf = [0; HASH_CHARS];
    base32::encode_into(&self.0, &mut buf);
    f.write_str(unsafe { std::str::from_utf8_unchecked(&buf) })
  }
}

impl Debug for Hash {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_tuple("Hash").field(&self.to_string()).finish()
  }
}

impl PartialOrd for Hash {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    Some(self.cmp(other))
  }
}

impl Ord for Hash {
  fn cmp(&self, other: &Self) -> Ordering {
    // base32 hashes are encoded backwards, so we sort them backwards too
    self.0.iter().rev().cmp(other.0.iter().rev())
  }
}

impl Deref for Hash {
  type Target = [u8];

  fn deref(&self) -> &Self::Target {
    &self.0[..]
  }
}

#[derive(Clone, Hash, Ord, PartialOrd, Eq, PartialEq, Debug, Display)]
pub struct Name(String);

impl Name {
  pub fn decode<S: AsRef<str>>(s: S) -> Result<Self> {
    fn is_valid_char(c: char) -> bool {
      c.is_ascii_alphabetic()
        || c.is_ascii_digit()
        || c == '+'
        || c == '-'
        || c == '.'
        || c == '_'
        || c == '?'
        || c == '='
    }

    let s = s.as_ref();
    ensure!(!s.is_empty(), "store path name cannot be empty");
    ensure!(s.len() <= 211, "store path name is too long");
    if s.starts_with('.') || s.chars().any(|x| !is_valid_char(x)) {
      bail!("invalid characters in store path name");
    }
    Ok(Self(s.into()))
  }
}

impl FromStr for Name {
  type Err = anyhow::Error;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Self::decode(s)
  }
}

impl Deref for Name {
  type Target = str;

  fn deref(&self) -> &Self::Target {
    &*self.0
  }
}
