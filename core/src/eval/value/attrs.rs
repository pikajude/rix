use std::borrow::Borrow;

use crate::util::*;

use super::Value;

#[derive(Clone, Debug)]
pub struct Attr {
  pub pos: Pos,
  pub name: Ident,
  pub value: Value,
}

#[derive(Debug, Default, Clone)]
pub struct Attrs(Vec<Attr>);

impl Attrs {
  pub fn get<Q: Ord>(&self, k: &Q) -> Option<&Attr>
  where
    Ident: Borrow<Q>,
  {
    if let Ok(i) = self.0.binary_search_by(|v| v.name.borrow().cmp(k)) {
      Some(&self.0[i])
    } else {
      None
    }
  }

  pub fn remove<Q: Ord>(&mut self, k: &Q) -> Option<Attr>
  where
    Ident: Borrow<Q>,
  {
    if let Ok(i) = self.0.binary_search_by(|v| v.name.borrow().cmp(k)) {
      Some(self.0.remove(i))
    } else {
      None
    }
  }

  pub fn new() -> Self {
    Self::default()
  }

  pub fn insert<N: Into<Ident>>(&mut self, name: N, pos: Pos, value: Value) -> Option<&Attr> {
    let new_attr = Attr {
      pos,
      name: name.into(),
      value,
    };
    self.insert_attr(new_attr)
  }

  fn insert_attr(&mut self, new_attr: Attr) -> Option<&Attr> {
    match self.0.binary_search_by(|v| v.name.cmp(&new_attr.name)) {
      Ok(i) => Some(&self.0[i]),
      Err(i) => {
        self.0.insert(i, new_attr);
        None
      }
    }
  }

  pub fn len(&self) -> usize {
    self.0.len()
  }

  pub fn is_empty(&self) -> bool {
    self.0.is_empty()
  }

  pub fn iter(&self) -> std::slice::Iter<Attr> {
    self.0.iter()
  }

  pub fn append(&mut self, other: &mut Self) {
    for item in other.0.drain(..) {
      self.insert_attr(item);
    }
  }
}

impl From<Vec<Attr>> for Attrs {
  fn from(mut v: Vec<Attr>) -> Self {
    v.sort_unstable_by(|m, n| m.name.cmp(&n.name));
    Self(v)
  }
}

impl<'a> IntoIterator for &'a Attrs {
  type Item = &'a Attr;

  type IntoIter = std::slice::Iter<'a, Attr>;

  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

impl IntoIterator for Attrs {
  type Item = Attr;

  type IntoIter = std::vec::IntoIter<Attr>;

  fn into_iter(self) -> Self::IntoIter {
    self.0.into_iter()
  }
}
