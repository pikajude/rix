use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use parking_lot::Mutex;

use self::builtins::Init;
use self::value::*;
use crate::syntax::expr::{AttrName, Bin, ExprType, Lambda, LambdaArg};
use crate::syntax::Expr;
use crate::util::error::Catchable;
use crate::util::*;

mod builtins;
pub mod value;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct CoerceOpts {
  coerce_more: bool,
  copy_to_store: bool,
}

impl Default for CoerceOpts {
  fn default() -> Self {
    Self {
      coerce_more: false,
      copy_to_store: true,
    }
  }
}

pub struct Eval {
  base_env: Env,
  cache: Mutex<HashMap<PathBuf, Value>>,
  #[allow(dead_code)]
  store: (),
}

impl Eval {
  pub fn new(store: ()) -> Self {
    Self {
      store,
      base_env: Env::new(Init::new().init()),
      cache: Default::default(),
    }
  }

  pub fn eval_inline<I: AsRef<str>>(&self, input: I) -> Result<Value> {
    let expr = crate::syntax::parse_inline(input.as_ref())?;
    self.eval(&self.base_env, &expr)
  }

  pub fn eval_file<P: AsRef<Path>>(&self, path: P) -> Result<Value> {
    fn inner(this: &Eval, path: &Path) -> Result<Value> {
      let real_path = if path.is_dir() {
        path.join("default.nix")
      } else {
        path.to_path_buf()
      };

      {
        if let Some(e) = this.cache.lock().get(path) {
          return Ok(e.clone());
        }
      }

      let expr = crate::syntax::parse_from_file(&real_path)?;
      debug!("evaluating file {}", real_path.display());
      let val = this.eval(&this.base_env, &expr)?;

      this.cache.lock().insert(path.to_path_buf(), val.clone());

      Ok(val)
    }

    inner(self, path.as_ref())
  }

  fn eval(&self, env: &Env, expr: &Expr) -> Result<Value> {
    match expr {
      Expr::Int { n } => Ok(Value::Prim(Prim::Int(*n))),
      Expr::Float { f } => Ok(Value::Prim(Prim::Float(*f))),
      Expr::Var { pos, name } => match self.lookup(*pos, env, name, false)? {
        Some(v) => Ok(v),
        None => throw!(*pos, "undefined variable `{}'", name),
      },
      Expr::String { s } => Ok(Value::string(s)),
      Expr::Path { path } => Ok(Value::Path(path.clone())),
      Expr::Apply { pos, lhs, rhs } => self.call_function(
        *pos,
        self.thunkify(*pos, env, lhs)?,
        self.thunkify(*pos, env, rhs)?,
      ),
      Expr::Lambda(l) => Ok(Value::Lambda(env.clone(), l.clone())),
      Expr::With {
        pos,
        env: env2,
        body,
      } => {
        let new_env = self.thunkify(*pos, env, env2)?;
        self.eval(&env.with(new_env), body)
      }
      Expr::Let { attrs, body } => {
        let mut new_scope = Arc::new(Attrs::default());
        let new_env = env.cons(Scope::Dynamic(Value::Attrs(new_scope.clone())));
        let mut scope = Attrs::default();
        for (key, value) in &attrs.attrs {
          scope.insert(
            key.clone(),
            value.pos,
            self.thunkify(
              value.pos,
              if value.inherited { env } else { &new_env },
              &value.rhs,
            )?,
          );
        }
        unsafe {
          *Arc::get_mut_unchecked(&mut new_scope) = scope;
        }
        self.eval(&new_env, body)
      }
      Expr::If {
        pos,
        cond,
        rhs1,
        rhs2,
      } => match self.eval(env, cond)? {
        Value::Prim(Prim::Bool(x)) => {
          if x {
            self.eval(env, rhs1)
          } else {
            self.eval(env, rhs2)
          }
        }
        v => throw!(*pos, "expected bool, got {}", v.typename()),
      },
      Expr::Op { bin, pos, lhs, rhs } => self.eval_op(env, *bin, *pos, lhs, rhs),
      Expr::Not { pos, e } => Ok(Value::bool(!self.eval_bool(*pos, env, e)?)),
      Expr::HasAttr { lhs, path } => {
        let mut lhs = self.eval(env, lhs)?;
        for key in path {
          lhs = self.force(key.pos, lhs)?;
          if let Some(attrs) = lhs.as_attrs() {
            let ident = self.get_name(key.pos, env, &key.v)?;
            if let Some(val) = attrs.get(&ident) {
              lhs = val.value.clone();
              continue;
            }
          }
          return Ok(Value::bool(false));
        }
        Ok(Value::bool(true))
      }
      Expr::Attrs(a) => self.build_attrs(env, a),
      Expr::Select {
        pos,
        lhs,
        def,
        path,
      } => {
        let mut pos = *pos;
        let mut vtmp = self.eval(env, lhs)?;
        for i in path {
          let sym = self.get_name(pos, env, &i.v)?;
          if let Some(def) = def {
            vtmp = self.force(pos, vtmp)?;
            if let Some(new_value) = vtmp.as_attrs().and_then(|x| x.get(&sym)) {
              pos = new_value.pos;
              vtmp = new_value.value.clone();
              continue;
            } else {
              return self.eval(env, def);
            }
          } else if let Some(val) = self.force_attrs(pos, vtmp)?.get(&sym) {
            pos = val.pos;
            vtmp = val.value.clone();
          } else {
            throw!(pos, "attribute `{}' missing", sym);
          }
        }
        Ok(vtmp)
      }
      Expr::Assert { pos, cond, body } => {
        if !self.eval_bool(*pos, env, cond)? {
          bail!(Catchable::Assert(*pos));
        }
        self.eval(env, body)
      }
      e => todo!("Expr::{:?} {:#}", ExprType::from(e), e),
    }
  }

  fn thunkify(&self, pos: Pos, env: &Env, expr: &Expr) -> Result<Value> {
    Ok(match expr {
      Expr::Int { n } => Value::int(*n),
      Expr::Float { f } => Value::float(*f),
      Expr::String { s } => Value::string(s),
      Expr::Path { path } => Value::Path(path.into()),
      Expr::Var { name, .. } => match self.lookup(pos, env, name, true)? {
        Some(v) => v,
        None => Value::Thunk(Arc::new(Thunk {
          e: expr.clone(),
          env: env.clone(),
        })),
      },
      _ => Value::Thunk(Arc::new(Thunk {
        e: expr.clone(),
        env: env.clone(),
      })),
    })
  }

  fn force(&self, _pos: Pos, value: Value) -> Result<Value> {
    if let Value::Thunk(t) = value {
      self.eval(&t.env, &t.e)
    } else {
      Ok(value)
    }
  }

  fn force_attrs(&self, pos: Pos, value: Value) -> Result<Arc<Attrs>> {
    match self.force(pos, value)? {
      Value::Attrs(a) => Ok(a),
      v => throw!(pos, "expected attrset, got {}", v.typename()),
    }
  }

  fn force_list(&self, pos: Pos, value: Value) -> Result<Arc<Vec<Value>>> {
    match self.force(pos, value)? {
      Value::List(l) => Ok(l),
      v => throw!(pos, "expected list, got {}", v.typename()),
    }
  }

  fn force_string_no_context(&self, pos: Pos, value: Value) -> Result<String> {
    match self.force(pos, value)? {
      Value::String(Str { s, ctx }) => {
        if ctx.is_empty() {
          Ok(s)
        } else {
          throw!(pos, "string may not refer to a store path")
        }
      }
      v => throw!(pos, "expected string, got {}", v.typename()),
    }
  }

  fn call_function(&self, pos: Pos, fun: Value, arg: Value) -> Result<Value> {
    match self.force(pos, fun)? {
      Value::Primop(op, mut args) => {
        args.push(Box::new(arg));
        if args.len() == op.arity as usize {
          (op.fun)(self, pos, args)
        } else {
          Ok(Value::Primop(op, args))
        }
      }
      Value::Lambda(e, l) => self.call_lambda(pos, e, l, arg),
      v => throw!(pos, "cannot call a {} as a function", v.typename()),
    }
  }

  fn call_lambda(&self, pos: Pos, env: Env, lam: Lambda, arg: Value) -> Result<Value> {
    let new_env = env.cons(Scope::Dynamic(Value::null()));

    let mut fn_env = Attrs::default();

    match lam.arg {
      LambdaArg::Formals { name, formals } => {
        let fs = self.force_attrs(pos, arg)?;

        let mut attrs_used = 0;

        if let Some(n) = name {
          fn_env.insert(n, pos, Value::Attrs(fs.clone()));
        }

        for formal in &formals.formals {
          if let Some(argval) = fs.get(&formal.name) {
            attrs_used += 1;
            fn_env.insert(formal.name.clone(), argval.pos, argval.value.clone());
          } else if let Some(ref x) = formal.def {
            let fallback = self.thunkify(pos, &new_env, x)?;
            fn_env.insert(formal.name.clone(), Pos::none(), fallback);
          } else {
            throw!(
              pos,
              "{} called without required argument `{}'",
              lam.name,
              formal.name
            );
          }
        }

        if !formals.ellipsis && attrs_used != fs.len() {
          for arg in fs.iter() {
            if !formals.formals.iter().any(|x| x.name == arg.name) {
              throw!(
                pos,
                "{} called with unexpected argument `{}'",
                lam.name,
                arg.name
              );
            }
          }
          unreachable!()
        }
      }
      LambdaArg::Plain(n) => {
        fn_env.insert(n, pos, arg);
      }
    }

    *new_env.envs.first().unwrap().write() = Scope::Dynamic(Value::Attrs(Arc::new(fn_env)));

    self.eval(&new_env, &lam.body)
  }

  fn lookup(&self, pos: Pos, env: &Env, var: &Ident, skip_eval: bool) -> Result<Option<Value>> {
    macro_rules! l {
      ($attrs:expr) => {{
        if let Some(v) = $attrs.get(var) {
          return Ok(Some(v.value.clone()));
        }
      }};
    }

    for scope in env.envs.iter() {
      match &mut *scope.write() {
        Scope::Static(s) => l!(s),
        Scope::Dynamic(d) => {
          if !skip_eval {
            *d = self.force(pos, d.clone())?;
          }
          if let Value::Attrs(attrs) = d {
            l!(attrs)
          } else if skip_eval {
            break;
          }
        }
      }
    }

    Ok(None)
  }

  fn coerce_new_string(&self, pos: Pos, v: Value, opts: CoerceOpts) -> Result<Str> {
    let mut ctx = PathSet::new();
    let s = self.coerce_to_string(pos, v, &mut ctx, opts)?;
    Ok(Str { s, ctx })
  }

  fn coerce_to_string(
    &self,
    pos: Pos,
    v: Value,
    context: &mut PathSet,
    opts: CoerceOpts,
  ) -> Result<String> {
    Ok(match self.force(pos, v)? {
      Value::String(Str { ctx, s }) => {
        context.extend(ctx.iter().cloned());
        s
      }
      Value::Path(p) => {
        trace!("canonicalizing {}", p.display());
        let p = p.canonicalize()?;
        if opts.copy_to_store {
          todo!("copy to store")
        } else {
          p.display().to_string()
        }
      }
      v => todo!("{:?}", v),
    })
  }

  fn coerce_to_path(&self, pos: Pos, v: Value, context: &mut PathSet) -> Result<PathBuf> {
    let s = self.coerce_to_string(
      pos,
      v,
      context,
      CoerceOpts {
        copy_to_store: false,
        coerce_more: false,
      },
    )?;
    let p = PathBuf::from(s);
    if !p.starts_with("/") {
      throw!(
        pos,
        "string `{}' doesn't represent an absolute path",
        p.display()
      );
    }
    Ok(p)
  }

  fn eval_op(&self, env: &Env, bin: Bin, pos: Pos, lhs: &Expr, rhs: &Expr) -> Result<Value> {
    match bin {
      Bin::And => Ok(Value::bool(
        self.eval_bool(pos, env, lhs)? && self.eval_bool(pos, env, rhs)?,
      )),
      Bin::Or => Ok(Value::bool(
        self.eval_bool(pos, env, lhs)? || self.eval_bool(pos, env, rhs)?,
      )),
      Bin::Eq => Ok(Value::bool(self.eq_values(
        pos,
        &self.thunkify(pos, env, lhs)?,
        &self.thunkify(pos, env, rhs)?,
      )?)),
      Bin::Impl => Ok(Value::bool(
        !self.eval_bool(pos, env, lhs)? || self.eval_bool(pos, env, rhs)?,
      )),
      Bin::Update => {
        let mut a1 = self.eval_attrs(pos, env, lhs)?;
        let mut a2 = self.eval_attrs(pos, env, rhs)?;

        if a1.is_empty() {
          Ok(Value::Attrs(a2))
        } else if a2.is_empty() {
          Ok(Value::Attrs(a1))
        } else {
          Arc::make_mut(&mut a1).append(Arc::make_mut(&mut a2));
          Ok(Value::Attrs(a1))
        }
      }
      b => throw!(pos, "unhandled operator {}", b),
    }
  }

  fn eval_bool(&self, pos: Pos, env: &Env, expr: &Expr) -> Result<bool> {
    match self.eval(env, expr)? {
      Value::Prim(Prim::Bool(b)) => Ok(b),
      v => throw!(pos, "expected bool, got `{}'", v.typename()),
    }
  }

  fn eval_attrs(&self, pos: Pos, env: &Env, expr: &Expr) -> Result<Arc<Attrs>> {
    match self.eval(env, expr)? {
      Value::Attrs(a) => Ok(a),
      v => throw!(pos, "expected attrset, got `{}'", v.typename()),
    }
  }

  fn get_name(&self, pos: Pos, env: &Env, name: &AttrName) -> Result<Ident> {
    match name {
      AttrName::Static(e) => Ok(e.clone()),
      AttrName::Dynamic(d) => {
        let v = self.eval(env, d)?;
        let s = self.force_string_no_context(pos, v)?;
        Ok(Ident::from(s))
      }
    }
  }

  fn build_attrs(&self, env: &Env, attrs: &crate::syntax::expr::Attrs) -> Result<Value> {
    let mut new_attrs = Attrs::new();

    let mut dynamic_env = env;
    let env2;

    if attrs.recursive {
      let recursive_scope = Value::null();
      env2 = env.cons(Scope::Dynamic(recursive_scope));
      dynamic_env = &env2;

      for (k, v) in attrs.attrs.iter() {
        new_attrs.insert(
          k.clone(),
          v.pos,
          self.thunkify(v.pos, if v.inherited { env } else { &env2 }, &v.rhs)?,
        );
      }

      *env2.envs.first().unwrap().write() =
        Scope::Dynamic(Value::Attrs(Arc::new(new_attrs.clone())));
    } else {
      for (k, v) in attrs.attrs.iter() {
        new_attrs.insert(k.clone(), v.pos, self.thunkify(v.pos, env, &v.rhs)?);
      }
    }

    for item in attrs.dyn_attrs.iter() {
      let name_val = self.force(item.pos, self.eval(dynamic_env, &item.name)?)?;
      if matches!(name_val, Value::Prim(Prim::Null)) {
        continue;
      }
      let name = Ident::from(self.force_string_no_context(item.pos, name_val)?);
      if let Some(old) = new_attrs.get(&name) {
        throw!(
          item.pos,
          "dynamic attribute `{}' already defined at {}",
          name,
          old.pos
        );
      }
      new_attrs.insert(
        name,
        item.pos,
        self.thunkify(item.pos, dynamic_env, &item.value)?,
      );
    }

    Ok(Value::Attrs(Arc::new(new_attrs)))
  }

  fn eq_values(&self, pos: Pos, v1: &Value, v2: &Value) -> Result<bool> {
    let v1 = self.force(pos, v1.clone())?;
    let v2 = self.force(pos, v2.clone())?;

    if let Some(result) = numeric_op(
      &v1,
      &v2,
      |i1, i2| i1 == i2,
      |f1, f2| (f1 - f2).abs() < std::f64::EPSILON,
    ) {
      return Ok(result);
    }

    throw!(pos, "compare {} and {}", v1.typename(), v2.typename());
  }
}

fn numeric_op<T, F: Fn(i64, i64) -> T, G: Fn(f64, f64) -> T>(
  v1: &Value,
  v2: &Value,
  ifun: F,
  ffun: G,
) -> Option<T> {
  match (v1, v2) {
    (Value::Prim(Prim::Int(i1)), Value::Prim(Prim::Int(i2))) => Some(ifun(*i1, *i2)),
    (Value::Prim(Prim::Int(i1)), Value::Prim(Prim::Float(f2))) => Some(ffun(*i1 as _, *f2)),
    (Value::Prim(Prim::Float(f1)), Value::Prim(Prim::Int(i2))) => Some(ffun(*f1, *i2 as _)),
    (Value::Prim(Prim::Float(f1)), Value::Prim(Prim::Float(f2))) => Some(ffun(*f1, *f2)),
    _ => None,
  }
}

#[cfg(test)]
mod tests {
  use std::assert_matches::assert_matches;

  use super::*;

  macro_rules! assert_eval {
    ($e:expr, $($t:tt)+) => {
      assert_matches!(Eval::new(()).eval_inline($e)?, $($t)+)
    };
  }

  #[test]
  fn test_eval() -> NixResult<()> {
    crate::util::logger::init()?;

    assert_eval!("null", Value::Prim(Prim::Null));
    assert_eval!("true", Value::Prim(Prim::Bool(true)));
    assert_eval!("false", Value::Prim(Prim::Bool(false)));
    assert_eval!("1", Value::Prim(Prim::Int(1)));
    assert_eval!("1.5", Value::Prim(Prim::Float(x)) if x >= 1.0);

    assert_eval!(
      "(import <nixpkgs> {}).stdenv.cc",
      Value::Prim(Prim::Bool(true))
    );
    ok()
  }
}
