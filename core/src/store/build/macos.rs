use crossbeam::thread::Scope;

use crate::store::{Derivation, Store, StorePath};
use crate::util::*;

use super::queue::Queue;
use super::{FinishedChild, Message};

pub(super) fn build<S: Store + ?Sized>(
  store: &S,
  messages: &Queue<Message>,
  scope: &Scope<'_>,
  path: &StorePath,
  drv: &Derivation,
) -> Result<Option<FinishedChild>> {
  bail!("not yet implemented")
}
