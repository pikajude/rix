use crate::Eval;
use parking_lot::RwLock;
use rix_syntax::expr::*;
use rix_util::*;
use std::{
  collections::{BTreeMap, BTreeSet},
  fmt::{self, Debug, Formatter},
  path::PathBuf,
  sync::Arc,
};

pub type ValueRef = Arc<RwLock<Value>>;
pub type PathSet = BTreeSet<String>;
pub type Attrs = BTreeMap<Ident, Located<ValueRef>>;
pub type Env = ConsList<Scope>;

#[derive(Debug, Clone)]
pub enum Scope {
  Static(Attrs),
  Dynamic(ValueRef),
}

#[derive(Debug, Clone)]
pub enum Value {
  Null,
  Bool(bool),
  Int(i64),
  Float(f64),
  String(Str),
  Path(PathBuf),
  Attrs(Arc<Attrs>),
  List(Arc<Vec<ValueRef>>),
  Apply(ValueRef, ValueRef),
  Thunk(Thunk),
  Lambda(Env, Lambda),
  Primop(Primop, Vec<ValueRef>),
  Blackhole,
}

#[derive(Debug, Clone)]
pub struct Str {
  s: String,
  ctx: BTreeSet<String>,
}

#[derive(Debug)]
pub struct Thunk(ExprRef);

impl Clone for Thunk {
  fn clone(&self) -> Self {
    panic!("thunks may not be cloned")
  }
}

pub type PrimopFn = fn(&Eval, Pos, Vec<ValueRef>) -> Result<Value>;

#[derive(Clone)]
pub struct Primop {
  pub fun: PrimopFn,
  pub name: Ident,
  pub arity: u8,
}

impl Debug for Primop {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "<primop {}>", self.name)
  }
}
