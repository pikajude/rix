use super::{Hash, HashType};
use crypto::digest::Digest;

pub struct Context {
  ty: HashType,
  buf: Buf,
  len: usize,
}

enum Buf {
  Md5(crypto::md5::Md5),
  Sha1(crypto::sha1::Sha1),
  Sha256(crypto::sha2::Sha256),
  Sha512(crypto::sha2::Sha512),
}

macro_rules! do_impl {
  ($x:ident, $($t:tt)+) => {
    match $x.buf {
      Buf::Md5(m) => m.$($t)+,
      Buf::Sha1(m) => m.$($t)+,
      Buf::Sha256(m) => m.$($t)+,
      Buf::Sha512(m) => m.$($t)+,
    }
  };
  (mut $x:ident, $($t:tt)+) => {
    match $x.buf {
      Buf::Md5(ref mut m) => m.$($t)+,
      Buf::Sha1(ref mut m) => m.$($t)+,
      Buf::Sha256(ref mut m) => m.$($t)+,
      Buf::Sha512(ref mut m) => m.$($t)+,
    }
  }
}

impl Digest for Context {
  fn input(&mut self, input: &[u8]) {
    do_impl!(mut self, input(input));
    self.len += input.len();
  }

  fn result(&mut self, out: &mut [u8]) {
    do_impl!(mut self, result(out))
  }

  fn reset(&mut self) {
    do_impl!(mut self, reset());
    self.len = 0;
  }

  fn output_bits(&self) -> usize {
    do_impl!(self, output_bits())
  }

  fn block_size(&self) -> usize {
    do_impl!(self, block_size())
  }
}

impl Context {
  pub fn new(ty: HashType) -> Self {
    Self {
      ty,
      buf: match ty {
        HashType::MD5 => Buf::Md5(crypto::md5::Md5::new()),
        HashType::SHA1 => Buf::Sha1(crypto::sha1::Sha1::new()),
        HashType::SHA256 => Buf::Sha256(crypto::sha2::Sha256::new()),
        HashType::SHA512 => Buf::Sha512(crypto::sha2::Sha512::new()),
      },
      len: 0,
    }
  }
}

impl Context {
  pub fn finish(&mut self) -> (Hash, usize) {
    let mut bytes = [0; 64];
    self.result(&mut bytes);
    (
      Hash {
        ty: self.ty,
        len: self.ty.size(),
        data: bytes,
      },
      self.len,
    )
  }
}
