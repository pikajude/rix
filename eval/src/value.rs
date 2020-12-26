use crate::Eval;
use parking_lot::RwLock;
use rix_syntax::expr::*;
use rix_util::*;
use smallvec::SmallVec;
use std::{
  collections::{BTreeMap, BTreeSet},
  fmt::{self, Debug, Formatter},
  path::PathBuf,
  sync::Arc,
};

pub type ValueRef = Arc<RwLock<Value>>;
pub type PathSet = BTreeSet<String>;
pub type Attrs = BTreeMap<Ident, Located<ValueRef>>;
pub type PrimopArgs = SmallVec<[ValueRef; 3]>;

#[derive(Clone)]
pub struct Env {
  pub envs: ConsList<Scope>,
  pub with: ConsList<ValueRef>,
}

impl Env {
  pub fn new(attrs: Attrs) -> Self {
    Self {
      envs: ConsList::new().cons(Scope::Static(attrs)),
      with: ConsList::new(),
    }
  }

  pub fn cons(&self, item: Scope) -> Self {
    Self {
      envs: self.envs.cons(item),
      with: self.with.clone(),
    }
  }

  pub fn with(&self, env: ValueRef) -> Self {
    Self {
      envs: self.envs.clone(),
      with: self.with.cons(env),
    }
  }
}

#[derive(Clone)]
pub enum Scope {
  Static(Attrs),
  Dynamic(ValueRef),
}

impl Debug for Scope {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Self::Static(a) => f.debug_tuple("Static").field(&a.keys()).finish(),
      Self::Dynamic(d) => f
        .debug_tuple("Dynamic")
        .field(&d.read().as_attrs().map(|x| x.keys()))
        .finish(),
    }
  }
}

#[derive(Clone, EnumAsInner)]
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
  Primop(Primop, PrimopArgs),
  Blackhole,
}

impl Value {
  pub fn string<S: Into<String>>(s: S) -> Self {
    Self::String(Str {
      s: s.into(),
      ctx: Default::default(),
    })
  }

  pub fn typename(&self) -> &'static str {
    match self {
      Self::Null => "null",
      Self::Bool(_) => "bool",
      Self::Int(_) => "int",
      Self::Float(_) => "float",
      Self::String(_) => "string",
      Self::Path(_) => "path",
      Self::Attrs(_) => "attrset",
      Self::List(_) => "list",
      Self::Apply(_, _) => "function application",
      Self::Thunk(_) => "thunk",
      Self::Lambda(_, _) => "lambda",
      Self::Primop(_, _) => "primop",
      Self::Blackhole => "blackhole",
    }
  }

  pub fn needs_eval(&self) -> bool {
    matches!(
      self,
      Self::Thunk { .. } | Self::Apply { .. } | Self::Blackhole
    )
  }
}

#[derive(Debug, Clone)]
pub struct Str {
  pub s: String,
  pub ctx: BTreeSet<String>,
}

pub struct Thunk(pub Env, pub ExprRef);

impl Thunk {
  pub fn new(env: &Env, expr: &Expr) -> Self {
    Self(env.clone(), Arc::new(expr.clone()))
  }
}

impl Clone for Thunk {
  fn clone(&self) -> Self {
    panic!("thunks may not be cloned")
  }
}

pub type PrimopFn = fn(&Eval, Pos, PrimopArgs) -> Result<Value>;

#[derive(Clone, Copy)]
pub struct Primop {
  pub fun: PrimopFn,
  pub name: &'static str,
  pub arity: u8,
}

impl Debug for Primop {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    write!(f, "<primop {}>", self.name)
  }
}
