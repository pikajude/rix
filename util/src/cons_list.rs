use std::fmt::{self, Debug, Formatter};
use std::iter::FusedIterator;
use std::sync::Arc;

struct Node<T> {
  elem: T,
  next: Option<Arc<Node<T>>>,
}

/// A Lisp-style cons list using [`Arc`] pointers.
///
/// This type doesn't have a [`FromIterator`](std::iter::FromIterator)
/// implementation since it would necessitate building the list in reverse. Use
/// the `collect` and/or `collect_reverse` associated functions instead.
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

  pub fn collect<D: DoubleEndedIterator<Item = T>>(collection: D) -> Self {
    Self::collect_reverse(collection.rev())
  }

  pub fn collect_reverse<I: Iterator<Item = T>>(iter: I) -> Self {
    let mut this = Self::new();
    for item in iter {
      this = this.cons(item);
    }
    this
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
