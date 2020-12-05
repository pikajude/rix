use crate::prelude::*;
use crypto::digest::Digest;
use std::{
  borrow::Cow,
  fmt::{self, Debug},
  fs::File,
  hash,
  ops::Deref,
  path::Path,
  str::FromStr,
};

mod context;
mod sink;

pub use context::Context;
pub use sink::Sink;

#[derive(Clone, Copy)]
pub struct Hash {
  data: [u8; 64],
  len: usize,
  ty: HashType,
}

impl Hash {
  fn len_base16(&self) -> usize {
    len_base16(self.len)
  }

  fn len_base32(&self) -> usize {
    len_base32(self.len)
  }

  fn len_base64(&self) -> usize {
    len_base64(self.len)
  }

  pub fn size(&self) -> usize {
    self.len
  }

  pub fn ty(&self) -> HashType {
    self.ty
  }

  #[inline]
  pub fn as_bytes(&self) -> &[u8] {
    &self.data[..self.len]
  }

  pub fn hash<S: AsRef<[u8]>>(bytes: S, ty: HashType) -> Self {
    let mut c = Context::new(ty);
    c.input(bytes.as_ref());
    c.finish().0
  }

  pub fn hash_file<P: AsRef<Path>>(path: P, ty: HashType) -> Result<(Self, usize)> {
    let mut ctx = Sink::new(ty);
    std::io::copy(&mut File::open(path)?, &mut ctx)?;
    Ok(ctx.finish())
  }

  pub fn new_allow_empty(s: &str, ty: Option<HashType>) -> Result<Self> {
    if s.is_empty() {
      if let Some(ht) = ty {
        Ok(Self {
          data: [0; 64],
          len: ht.size(),
          ty: ht,
        })
      } else {
        bail!("an empty hash may not be untyped")
      }
    } else {
      match ty {
        Some(ht) => {
          if s.contains(|x| x == ':' || x == '-') {
            let h = Self::decode(s)?;
            ensure!(h.ty() == ht, "expected hash type {}, got {}", ht, h.ty());
            Ok(h)
          } else {
            Self::decode_with_type(s, ht, false)
          }
        }
        None => Self::decode(s),
      }
    }
  }

  /// Encode to serialized representation
  pub fn encode(&self, encoding: Encoding) -> String {
    if encoding == Encoding::SRI {
      return self.encode_with_type(encoding);
    }
    let mut s = String::new();
    self.encode_impl(encoding, &mut s);
    s
  }

  pub fn encode_with_type(&self, encoding: Encoding) -> String {
    let mut s = self.ty.to_string();
    if encoding == Encoding::SRI {
      s.push('-');
    } else {
      s.push(':');
    }
    self.encode_impl(encoding, &mut s);
    s
  }

  fn encode_impl(&self, encoding: Encoding, buf: &mut String) {
    let bytes = match encoding {
      Encoding::Base16 => {
        let mut bytes = vec![0; self.len_base16()];
        binascii::bin2hex(self.as_bytes(), &mut bytes).expect("Incorrect buffer size");
        bytes
      }
      Encoding::Base32 => {
        let mut bytes = vec![0; self.len_base32()];
        base32::encode_into(self.as_bytes(), &mut bytes);
        bytes
      }
      Encoding::Base64 | Encoding::SRI => {
        let mut bytes = vec![0; self.len_base64()];
        binascii::b64encode(self.as_bytes(), &mut bytes).expect("Incorrect buffer size");
        bytes
      }
    };
    buf.push_str(unsafe { std::str::from_utf8_unchecked(&bytes) });
  }

  pub fn decode<S: AsRef<str>>(input: S) -> Result<Self> {
    let input = input.as_ref();
    if let Some((ty, rest)) = break_str(input, ':') {
      Ok(Self::decode_with_type(rest, ty.parse()?, false)?)
    } else if let Some((ty, rest)) = break_str(input, '-') {
      Ok(Self::decode_with_type(rest, ty.parse()?, true)?)
    } else {
      bail!("untyped input hash")
    }
  }

  pub fn decode_with_type(input: &str, ty: HashType, sri: bool) -> Result<Self> {
    let mut bytes = [0; 64];
    if !sri && input.len() == len_base16(ty.size()) {
      binascii::hex2bin(input.as_bytes(), &mut bytes).map_err(|e| anyhow::anyhow!("{:?}", e))?;
      Ok(Self {
        data: bytes,
        ty,
        len: ty.size(),
      })
    } else if !sri && input.len() == len_base32(ty.size()) {
      base32::decode_into(input.as_bytes(), &mut bytes)?;
      Ok(Self {
        data: bytes,
        ty,
        len: ty.size(),
      })
    } else if sri || input.len() == len_base64(ty.size()) {
      base64::decode_config_slice(input, base64::STANDARD, &mut bytes)?;
      Ok(Self {
        data: bytes,
        ty,
        len: ty.size(),
      })
    } else {
      bail!("invalid serialized hash")
    }
  }

  pub fn truncate(&self, new_size: usize) -> Cow<Self> {
    if new_size >= self.len {
      return Cow::Borrowed(self);
    }
    let mut data = [0; 64];
    for i in 0..self.len {
      data[i % new_size] ^= self.data[i];
    }
    Cow::Owned(Self {
      len: new_size,
      data,
      ty: self.ty,
    })
  }
}

impl PartialEq for Hash {
  fn eq(&self, other: &Self) -> bool {
    self.ty == other.ty && self.as_bytes() == other.as_bytes()
  }
}

impl Eq for Hash {}

impl hash::Hash for Hash {
  fn hash<H: hash::Hasher>(&self, state: &mut H) {
    self.as_bytes().hash(state)
  }
}

impl Deref for Hash {
  type Target = [u8];

  fn deref(&self) -> &Self::Target {
    self.as_bytes()
  }
}

impl Debug for Hash {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.debug_tuple("Hash")
      .field(&format!("{}:{}", self.ty, self.encode(Encoding::Base32)))
      .finish()
  }
}

fn len_base16(size: usize) -> usize {
  size * 2
}

fn len_base32(size: usize) -> usize {
  (size * 8 - 1) / 5 + 1
}

fn len_base64(size: usize) -> usize {
  ((4 * size / 3) + 3) & !3
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Display)]
pub enum HashType {
  #[display(fmt = "md5")]
  MD5,
  #[display(fmt = "sha1")]
  SHA1,
  #[display(fmt = "sha256")]
  SHA256,
  #[display(fmt = "sha512")]
  SHA512,
}

impl HashType {
  fn size(self) -> usize {
    match self {
      Self::MD5 => 16,
      Self::SHA1 => 20,
      Self::SHA256 => 32,
      Self::SHA512 => 64,
    }
  }
}

impl FromStr for HashType {
  type Err = anyhow::Error;

  fn from_str(s: &str) -> Result<Self, Self::Err> {
    Ok(match s {
      "md5" => Self::MD5,
      "sha1" => Self::SHA1,
      "sha256" => Self::SHA256,
      "sha512" => Self::SHA512,
      x => bail!("invalid hash type `{}'", x),
    })
  }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Encoding {
  Base64,
  Base32,
  Base16,
  SRI,
}
