use std::collections::BTreeSet;
use std::path::PathBuf;
use std::sync::Arc;

use parking_lot::RwLock;
use smallvec::SmallVec;

use crate::syntax::expr::Lambda;
use crate::syntax::Expr;
use crate::util::*;

pub use self::attrs::Attrs;
use super::Eval;

mod attrs;

pub type PathSet = BTreeSet<String>;

#[derive(Clone)]
pub struct Env {
  pub envs: ConsList<RwLock<Scope>>,
  pub with: ConsList<RwLock<Value>>,
}

impl std::fmt::Debug for Env {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(
      f,
      "<env scopes:{} with-scopes:{}>",
      self.envs.len(),
      self.with.len()
    )
  }
}

impl Env {
  pub fn new(attrs: Attrs) -> Self {
    Self {
      envs: ConsList::new().cons(RwLock::new(Scope::Static(attrs))),
      with: ConsList::new(),
    }
  }

  pub fn cons(&self, item: Scope) -> Self {
    Self {
      envs: self.envs.cons(RwLock::new(item)),
      with: self.with.clone(),
    }
  }

  pub fn with(&self, env: Value) -> Self {
    Self {
      envs: self.envs.clone(),
      with: self.with.cons(RwLock::new(env)),
    }
  }
}

#[derive(Clone, Debug)]
pub enum Scope {
  Static(Attrs),
  Dynamic(Value),
}

#[derive(Debug)]
pub struct Thunk {
  pub env: Env,
  pub e: Expr,
}

#[derive(Clone, Debug)]
pub struct Str {
  pub s: String,
  pub ctx: BTreeSet<String>,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Prim {
  Null,
  Bool(bool),
  Int(i64),
  Float(f64),
}

impl Prim {
  pub fn typename(&self) -> &'static str {
    match self {
      Self::Null => "null",
      Self::Bool(_) => "bool",
      Self::Int(_) => "int",
      Self::Float(_) => "float",
    }
  }
}

pub type PrimopArgs = SmallVec<[Box<Value>; 3]>;
pub type PrimopFn = fn(&Eval, Pos, PrimopArgs) -> Result<Value>;

#[derive(Clone, Copy)]
pub struct Primop {
  pub fun: PrimopFn,
  pub name: &'static str,
  pub arity: u8,
}

impl std::fmt::Debug for Primop {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    write!(f, "<primop {}>", self.name)
  }
}

#[derive(Clone, Debug, EnumAsInner)]
pub enum Value {
  Prim(Prim),
  String(Str),
  Path(PathBuf),
  List(Arc<Vec<Value>>),
  Attrs(Arc<Attrs>),
  Lambda(Env, Lambda),
  Primop(Primop, PrimopArgs),
  Thunk(Arc<Thunk>),
}

impl Value {
  #[inline]
  pub fn null() -> Self {
    Self::Prim(Prim::Null)
  }
  #[inline]
  pub fn bool(b: bool) -> Self {
    Self::Prim(Prim::Bool(b))
  }

  pub fn int<I: Into<i64>>(i: I) -> Self {
    Self::Prim(Prim::Int(i.into()))
  }

  pub fn float<I: Into<f64>>(i: I) -> Self {
    Self::Prim(Prim::Float(i.into()))
  }

  pub fn string<S: Into<String>>(s: S) -> Self {
    Self::String(Str {
      s: s.into(),
      ctx: Default::default(),
    })
  }

  pub fn typename(&self) -> &'static str {
    match self {
      Self::Prim(p) => p.typename(),
      Self::String(Str { ctx, .. }) => {
        if ctx.is_empty() {
          "string"
        } else {
          "string with context"
        }
      }
      Self::Path(_) => "path",
      Self::List(_) => "list",
      Self::Attrs(_) => "attrset",
      Self::Lambda { .. } => "lambda",
      Self::Primop { .. } => "primop",
      Self::Thunk(_) => "thunk",
    }
  }
}
