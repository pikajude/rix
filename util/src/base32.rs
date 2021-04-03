use crate::*;

static B32_CHARS: [u8; 32] = *b"0123456789abcdfghijklmnpqrsvwxyz";

lazy_static! {
  static ref B32_CHARS_REV: [u8; 256] = {
    let mut xs = [0xffu8; 256];
    for (n, c) in B32_CHARS.iter().enumerate() {
      xs[*c as usize] = n as u8;
    }
    xs
  };
  static ref IS_BASE32: [bool; 256] = {
    let mut isb32 = [false; 256];
    for c in B32_CHARS.iter() {
      isb32[*c as usize] = true;
    }
    isb32
  };
}

pub fn is_base32(c: char) -> bool {
  if c > 'z' {
    false
  } else {
    IS_BASE32[c as usize]
  }
}

pub trait CharIsBase32 {
  fn is_base32(&self) -> bool;
}

impl CharIsBase32 for char {
  fn is_base32(&self) -> bool {
    is_base32(*self)
  }
}

impl CharIsBase32 for u8 {
  fn is_base32(&self) -> bool {
    is_base32(*self as char)
  }
}

fn encode_len(i: usize) -> usize {
  if i == 0 {
    0
  } else {
    (i * 8 - 1) / 5 + 1
  }
}

fn decode_len(i: usize) -> usize {
  i * 5 / 8
}

pub fn encode(input: &[u8]) -> Vec<u8> {
  let mut buf = vec![0; encode_len(input.len())];
  encode_into(input, &mut buf);
  buf
}

pub fn encode_into(input: &[u8], output: &mut [u8]) {
  let len = encode_len(input.len());
  assert_eq!(len, output.len());

  let mut nr_bits_left: usize = 0;
  let mut bits_left: u16 = 0;
  let mut pos = len;

  for b in input {
    bits_left |= (*b as u16) << nr_bits_left;
    nr_bits_left += 8;
    while nr_bits_left > 5 {
      output[pos - 1] = B32_CHARS[(bits_left & 0x1f) as usize];
      pos -= 1;
      bits_left >>= 5;
      nr_bits_left -= 5;
    }
  }

  if nr_bits_left > 0 {
    output[pos - 1] = B32_CHARS[(bits_left & 0x1f) as usize];
    pos -= 1;
  }

  assert_eq!(pos, 0);
}

pub fn decode_into(input: &[u8], out: &mut [u8]) -> Result<()> {
  let mut nr_bits_left: usize = 0;
  let mut bits_left: u16 = 0;

  let mut ix = 0;

  for c in input.iter().copied().rev() {
    let b = B32_CHARS_REV[c as usize];
    ensure!(b != 0xff, "invalid base32 character `{}'", c);
    bits_left |= (b as u16) << nr_bits_left;
    nr_bits_left += 5;
    if nr_bits_left >= 8 {
      out[ix] = bits_left as u8;
      ix += 1;
      bits_left >>= 8;
      nr_bits_left -= 8;
    }
  }

  if nr_bits_left > 0 && bits_left != 0 {
    bail!("base32 input: invalid length")
  }

  Ok(())
}

pub fn decode(input: &[u8]) -> Result<Vec<u8>> {
  let mut res = vec![0; decode_len(input.len())];

  decode_into(input, &mut res)?;

  Ok(res)
}

#[cfg(test)]
mod tests {
  use super::*;
  use proptest::*;

  #[test]
  fn test_encode() {
    assert_eq!(encode(&[]), b"");

    assert_eq!(
      encode(&hex::decode("0839703786356bca59b0f4a32987eb2e6de43ae8").unwrap()),
      b"x0xf8v9fxf3jk8zln1cwlsrmhqvp0f88"
    );

    assert_eq!(
      encode(
        &hex::decode("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad").unwrap()
      ),
      &b"1b8m03r63zqhnjf7l5wnldhh7c134ap5vpj0850ymkq1iyzicy5s"[..]
    );

    // rustfmt doesn't bother formatting these because they're too long
    let longhex = "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f";
    let long32 = b"2gs8k559z4rlahfx0y688s49m2vvszylcikrfinm30ly9rak69236nkam5ydvly1ai7xac99vxfc4ii84hawjbk876blyk1jfhkbbyx";

    assert_eq!(encode(&hex::decode(longhex).unwrap()), &long32[..]);
  }

  #[test]
  fn test_decode() {
    assert_eq!(hex::encode(decode(b"").unwrap()), "");

    assert_eq!(
      hex::encode(decode(b"x0xf8v9fxf3jk8zln1cwlsrmhqvp0f88").unwrap()),
      "0839703786356bca59b0f4a32987eb2e6de43ae8"
    );

    assert_eq!(
      hex::encode(decode(b"1b8m03r63zqhnjf7l5wnldhh7c134ap5vpj0850ymkq1iyzicy5s").unwrap()),
      "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
    );

    assert_eq!(
      hex::encode(decode(b"2gs8k559z4rlahfx0y688s49m2vvszylcikrfinm30ly9rak69236nkam5ydvly1ai7xac99vxfc4ii84hawjbk876blyk1jfhkbbyx").unwrap()), 
      "ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"
    );

    assert_matches!(decode(b"xoxf8v9fxf3jk8zln1cwlsrmhqvp0f88"), Err(_));
    assert_matches!(
      decode(b"2b8m03r63zqhnjf7l5wnldhh7c134ap5vpj0850ymkq1iyzicy5s"),
      Err(_)
    );
    assert_matches!(decode(b"2"), Err(_));
    assert_matches!(decode(b"2gs"), Err(_));
    assert_matches!(decode(b"2gs8"), Err(_));
  }

  proptest! {
    #[test]
    fn roundtrip(s: Vec<u8>) {
      assert_eq!(s, decode(&encode(&s)).unwrap());
    }
  }
}
