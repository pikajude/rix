use std::{
  fmt::{self, Debug, Formatter},
  iter::FusedIterator,
  sync::Arc,
};

struct Node<T> {
  elem: T,
  next: Option<Arc<Node<T>>>,
}

pub struct ConsList<T> {
  front: Option<Arc<Node<T>>>,
  length: usize,
}

impl<T> ConsList<T> {
  pub fn new() -> Self {
    Self {
      front: None,
      length: 0,
    }
  }

  pub fn cons(&self, elem: T) -> Self {
    let new_node = Node {
      elem,
      next: self.front.clone(),
    };
    Self {
      front: Some(Arc::new(new_node)),
      length: self.length + 1,
    }
  }

  pub fn first(&self) -> Option<&T> {
    self.front.as_ref().map(|x| &x.elem)
  }

  pub fn iter(&self) -> Iter<T> {
    Iter {
      head: self.front.as_deref(),
      len: self.length,
    }
  }

  #[inline]
  pub fn len(&self) -> usize {
    self.length
  }

  #[inline]
  pub fn is_empty(&self) -> bool {
    self.length == 0
  }
}

impl<T> Default for ConsList<T> {
  fn default() -> Self {
    Self::new()
  }
}

impl<T> Clone for ConsList<T> {
  fn clone(&self) -> Self {
    Self {
      front: self.front.clone(),
      length: self.length,
    }
  }
}

impl<T: Debug> Debug for ConsList<T> {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    f.debug_list().entries(self.iter()).finish()
  }
}

impl<'a, T> IntoIterator for &'a ConsList<T> {
  type IntoIter = Iter<'a, T>;
  type Item = &'a T;

  fn into_iter(self) -> Self::IntoIter {
    self.iter()
  }
}

impl<T> Drop for ConsList<T> {
  fn drop(&mut self) {
    // don't want to blow the stack with destructors,
    // but also don't want to walk the whole list.
    // So walk the list until we find a non-uniquely owned item
    let mut head = self.front.take();
    loop {
      let temp = head;
      match temp {
        Some(node) => match Arc::try_unwrap(node) {
          Ok(mut node) => {
            head = node.next.take();
          }
          _ => return,
        },
        _ => return,
      }
    }
  }
}

pub struct Iter<'a, T> {
  head: Option<&'a Node<T>>,
  len: usize,
}

impl<'a, T> Iterator for Iter<'a, T> {
  type Item = &'a T;

  fn next(&mut self) -> Option<Self::Item> {
    match self.head.take() {
      None => None,
      Some(head) => {
        self.len -= 1;
        self.head = head.next.as_deref();
        Some(&head.elem)
      }
    }
  }

  fn size_hint(&self) -> (usize, Option<usize>) {
    (self.len, Some(self.len))
  }
}

impl<'a, T> FusedIterator for Iter<'a, T> {}
impl<'a, T> ExactSizeIterator for Iter<'a, T> {}
