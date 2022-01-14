#![feature(async_closure, map_first_last, option_zip)]

extern crate rix_syntax as syntax;
extern crate rix_util as util;

use crate::syntax::expr::{AttrName, Bin, Lambda, LambdaArg};
use crate::syntax::Expr;
use crate::util::*;
use async_recursion::async_recursion;
use error::*;
use parking_lot::*;
use rix_store::{FileIngestionMethod, Repair, Store};
use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use value::*;

mod builtins;
mod derivation;
pub mod fetch;
mod print;
mod value;

pub fn vref(v: Value) -> ValueRef {
  Arc::new(RwLock::new(v))
}

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
  pub store: Arc<dyn Store>,
  eval_cache: Mutex<HashMap<PathBuf, ValueRef>>,
}

impl Eval {
  pub fn new(store: Arc<dyn Store>) -> Self {
    Self {
      base_env: Env::new(builtins::Init::new().init(&*store)),
      store,
      eval_cache: Default::default(),
    }
  }

  #[cfg(test)]
  pub fn test() -> Self {
    logger::init().expect("unable to init logger");
    Self::new(Arc::new(
      rix_core::local_store::LocalStore::new().expect("unable to open local store"),
    ))
  }

  pub async fn eval_file<P: AsRef<Path>>(&self, path: P) -> Result<Value> {
    let path = path.as_ref();
    let real_path = if path.is_dir() {
      path.join("default.nix")
    } else {
      path.to_path_buf()
    };

    {
      if let Some(f) = self.eval_cache.lock().get(path) {
        return self.clone_value(Pos::none(), f).await;
      }
    }

    let expr = Arc::new(crate::syntax::parse_from_file(&real_path)?);
    debug!("evaluating file {}", real_path.display());
    let val = vref(self.eval(&self.base_env, &expr).await?);

    self
      .eval_cache
      .lock()
      .insert(path.to_path_buf(), val.clone());

    self.clone_value(Pos::none(), &val).await
  }

  pub async fn eval_inline<I: AsRef<str>>(&self, input: I) -> Result<Value> {
    let expr = crate::syntax::parse_inline(input.as_ref())?;
    self.eval(&self.base_env, &expr).await
  }

  async fn defer(&self, pos: Pos, env: &Env, expr: &Expr) -> Result<ValueRef> {
    Ok(match expr {
      Expr::Int { n } => vref(Value::Int(*n)),
      Expr::Float { f } => vref(Value::Float(*f)),
      Expr::String { s } => vref(Value::string(s)),
      Expr::Path { path } => vref(Value::Path(path.into())),
      Expr::Var { name, .. } => match self.lookup(pos, env, name, true).await? {
        Some(v) => v,
        None => vref(Value::Thunk(Thunk::new(env, expr))),
      },
      e => vref(Value::Thunk(Thunk::new(env, e))),
    })
  }

  #[async_recursion(?Send)]
  async fn eval(&self, env: &Env, expr: &Expr) -> Result<Value> {
    match expr {
      Expr::Int { n } => Ok(Value::Int(*n)),
      Expr::Float { f } => Ok(Value::Float(*f)),
      Expr::String { s } => Ok(Value::string(s)),
      Expr::Path { path } => Ok(Value::Path(path.into())),
      Expr::Var { name, pos } => match self.lookup(*pos, env, name, false).await? {
        Some(v) => self.clone_value(*pos, &v).await,
        None => throw!(*pos, "undefined variable `{}'", name),
      },
      Expr::Let { attrs, body } => {
        let mut scope = Attrs::new();
        let new_scope = vref(Value::Blackhole);
        let new_env = env.cons(Scope::Dynamic(new_scope.clone()));
        for (key, value) in &attrs.attrs {
          scope.insert(
            key.clone(),
            Located {
              pos: value.pos,
              v: self
                .defer(
                  value.pos,
                  if value.inherited { env } else { &new_env },
                  &value.rhs,
                )
                .await?,
            },
          );
        }
        *new_scope.write() = Value::Attrs(Arc::new(scope));
        self.eval(&new_env, body).await
      }
      Expr::If {
        pos,
        cond,
        rhs1,
        rhs2,
      } => {
        if self.eval_bool(*pos, env, cond).await? {
          self.eval(env, rhs1).await
        } else {
          self.eval(env, rhs2).await
        }
      }
      Expr::With {
        pos,
        env: env2,
        body,
      } => {
        let new_env = self.defer(*pos, env, env2).await?;
        self.eval(&env.with(new_env), body).await
      }
      Expr::Op { bin, pos, lhs, rhs } => self.eval_op(env, *bin, *pos, lhs, rhs).await,
      Expr::Not { pos, e } => Ok(Value::Bool(!self.eval_bool(*pos, env, e).await?)),
      Expr::HasAttr { lhs, path } => {
        let mut lhs = vref(self.eval(env, lhs).await?);
        for key in path {
          let lhs_ = self.force(key.pos, &lhs).await?;
          if let Some(attrs) = lhs_.as_attrs() {
            let ident = self.get_name(key.pos, env, &key.v).await?;
            if let Some(val) = attrs.get(&ident) {
              let it = val.v.clone();
              drop(lhs_);
              lhs = it;
              continue;
            }
          }
          return Ok(Value::Bool(false));
        }
        Ok(Value::Bool(true))
      }
      Expr::Apply { pos, lhs, rhs } => {
        self
          .call_function(
            *pos,
            &self.defer(*pos, env, lhs).await?,
            &self.defer(*pos, env, rhs).await?,
          )
          .await
      }
      Expr::Lambda(l) => Ok(Value::Lambda(env.clone(), l.clone())),
      Expr::Select {
        pos,
        lhs,
        def,
        path,
      } => {
        let mut pos = *pos;
        let mut vtmp = vref(self.eval(env, lhs).await?);
        for i in path {
          let sym = self.get_name(pos, env, &i.v).await?;
          if let Some(def) = def {
            let vtmp2 = self.force(pos, &vtmp).await?;
            if let Some(new_value) = vtmp2.as_attrs().and_then(|x| x.get(&sym)).cloned() {
              drop(vtmp2);
              pos = new_value.pos;
              vtmp = new_value.v;
              continue;
            } else {
              return self.eval(env, def).await;
            }
          } else {
            let vtmp2 = self.force_attrs(pos, &vtmp).await?;
            if let Some(val) = vtmp2.get(&sym).cloned() {
              drop(vtmp2);
              pos = val.pos;
              vtmp = val.v;
            } else {
              throw!(pos, "attribute `{}' missing", sym)
            }
          }
        }
        self.clone_value(pos, &vtmp).await
      }
      Expr::Attrs(a) => self.build_attrs(env, a).await,
      Expr::Assert { pos, cond, body } => {
        if !self.eval_bool(*pos, env, cond).await? {
          bail!(Catchable::Assert(*pos));
        }
        self.eval(env, body).await
      }
      Expr::List(items) => {
        let mut list = vec![];
        for item in items {
          list.push(vref(self.eval(env, item).await?));
        }
        Ok(Value::List(Arc::new(list)))
      }
      Expr::ConcatStrings {
        pos,
        force_string,
        parts,
      } => self.concat_strings(*pos, *force_string, parts, env).await,
      e => todo!("{:?}", e),
    }
  }

  async fn eval_op(&self, env: &Env, bin: Bin, pos: Pos, lhs: &Expr, rhs: &Expr) -> Result<Value> {
    match bin {
      Bin::Or => Ok(Value::Bool(
        self.eval_bool(pos, env, lhs).await? || self.eval_bool(pos, env, rhs).await?,
      )),
      Bin::And => Ok(Value::Bool(
        self.eval_bool(pos, env, lhs).await? && self.eval_bool(pos, env, rhs).await?,
      )),
      Bin::Eq => Ok(Value::Bool(
        self
          .eq_values(
            pos,
            &self.defer(pos, env, lhs).await?,
            &self.defer(pos, env, rhs).await?,
          )
          .await?,
      )),
      Bin::Neq => Ok(Value::Bool(
        !self
          .eq_values(
            pos,
            &self.defer(pos, env, lhs).await?,
            &self.defer(pos, env, rhs).await?,
          )
          .await?,
      )),
      Bin::Impl => Ok(Value::Bool(
        !self.eval_bool(pos, env, lhs).await? || self.eval_bool(pos, env, rhs).await?,
      )),
      Bin::Update => {
        let mut attrs1 = self.eval_attrs(pos, env, lhs).await?;
        let mut attrs2 = self.eval_attrs(pos, env, rhs).await?;

        if attrs1.is_empty() {
          Ok(Value::Attrs(attrs2))
        } else if attrs2.is_empty() {
          Ok(Value::Attrs(attrs1))
        } else {
          Arc::make_mut(&mut attrs1).append(Arc::make_mut(&mut attrs2));
          Ok(Value::Attrs(attrs1))
        }
      }
      Bin::ConcatLists => {
        let mut list1 = self.eval_list(pos, env, lhs).await?;
        let list2 = self.eval_list(pos, env, rhs).await?;

        Arc::make_mut(&mut list1).extend(list2.iter().cloned());
        Ok(Value::List(list1))
      }
    }
  }

  async fn concat_strings(
    &self,
    pos: Pos,
    force_string: bool,
    parts: &[Expr],
    env: &Env,
  ) -> Result<Value> {
    enum ConcatTy {
      Int(i64),
      Float(f64),
      Path(PathBuf),
      String(String),
    }

    let mut ctx = PathSet::new();

    let (part0, partrest) = parts
      .split_first()
      .expect("empty list inside ConcatStrings");

    let part0 = self.eval(env, part0).await?;

    let should_copy = force_string || part0.as_string().is_some();

    let first = match part0 {
      Value::Int(i) => ConcatTy::Int(i),
      Value::Float(f) => ConcatTy::Float(f),
      Value::Path(p) => ConcatTy::Path(p),
      v => ConcatTy::String(
        self
          .coerce_to_string(
            pos,
            &vref(v),
            &mut ctx,
            CoerceOpts {
              coerce_more: false,
              copy_to_store: should_copy,
            },
          )
          .await?,
      ),
    };

    let mut vtmp = first;
    for part in partrest.iter() {
      let part = self.eval(env, part).await?;
      vtmp = match (vtmp, part) {
        (ConcatTy::Int(i0), Value::Int(i1)) => ConcatTy::Int(i0 + i1),
        (ConcatTy::Int(i0), Value::Float(i1)) => ConcatTy::Float((i0 as f64) + i1),
        (ConcatTy::Int(_), x) => throw!(pos, "cannot add {} to an integer", x.typename()),
        (ConcatTy::Float(f0), Value::Int(f1)) => ConcatTy::Float(f0 + (f1 as f64)),
        (ConcatTy::Float(f0), Value::Float(f1)) => ConcatTy::Float(f0 + f1),
        (ConcatTy::Float(_), x) => throw!(pos, "cannot add {} to a float", x.typename()),
        (ConcatTy::Path(p), x) => {
          let s = self
            .coerce_to_string(
              pos,
              &vref(x),
              &mut ctx,
              CoerceOpts {
                coerce_more: false,
                copy_to_store: should_copy,
              },
            )
            .await?;
          if !ctx.is_empty() {
            throw!(
              pos,
              "a string that refers to a store path cannot be appended to a path"
            );
          }
          ConcatTy::Path(p.join(s.strip_prefix('/').unwrap_or(&s)))
        }
        (ConcatTy::String(mut s0), x) => {
          let s = self
            .coerce_to_string(
              pos,
              &vref(x),
              &mut ctx,
              CoerceOpts {
                coerce_more: false,
                copy_to_store: should_copy,
              },
            )
            .await?;
          s0.push_str(&s);
          ConcatTy::String(s0)
        }
      };
    }

    Ok(match vtmp {
      ConcatTy::Int(i) => Value::Int(i),
      ConcatTy::Float(f) => Value::Float(f),
      ConcatTy::Path(p) => Value::Path(p),
      ConcatTy::String(s) => Value::String(Str { s, ctx }),
    })
  }

  async fn lookup(
    &self,
    pos: Pos,
    env: &Env,
    var: &Ident,
    no_eval: bool,
  ) -> Result<Option<ValueRef>> {
    macro_rules! do_lookup {
      ($attrs:expr) => {{
        if let Some(v) = $attrs.get(var) {
          return Ok(Some(v.v.clone()));
        }
      }};
    }

    for scope in &env.envs {
      match scope {
        Scope::Static(a) => do_lookup!(a),
        Scope::Dynamic(d) => {
          if !no_eval {
            let _e = self.force(pos, d).await?;
          }
          if let Some(a) = d.read().as_attrs() {
            do_lookup!(a)
          } else if no_eval {
            break;
          }
        }
      }
    }

    for d in &env.with {
      if !no_eval {
        let _e = self.force(pos, d).await?;
      }
      if let Some(d) = d.try_read() {
        if let Some(a) = d.as_attrs() {
          do_lookup!(a)
        }
      } else if no_eval {
        break;
      }
    }

    Ok(None)
  }

  async fn get_name(&self, pos: Pos, env: &Env, name: &AttrName) -> Result<Ident> {
    match name {
      AttrName::Static(e) => Ok(e.clone()),
      AttrName::Dynamic(d) => {
        let v = vref(self.eval(env, d).await?);
        let s = self.force_string_no_context(pos, &v).await?;
        Ok(Ident::from(&*s))
      }
    }
  }

  async fn build_attrs(&self, env: &Env, attrs: &crate::syntax::expr::Attrs) -> Result<Value> {
    let mut new_attrs = Attrs::new();
    let mut dynamic_env = env;
    let env2;
    if attrs.recursive {
      let recursive_scope = vref(Value::Blackhole);
      env2 = env.cons(Scope::Dynamic(recursive_scope.clone()));
      dynamic_env = &env2;

      for (k, v) in attrs.attrs.iter() {
        new_attrs.insert(
          k.clone(),
          Located {
            pos: v.pos,
            v: self
              .defer(v.pos, if v.inherited { env } else { &env2 }, &v.rhs)
              .await?,
          },
        );
      }

      *recursive_scope.write() = Value::Attrs(Arc::new(new_attrs.clone()));
    } else {
      for (k, v) in attrs.attrs.iter() {
        new_attrs.insert(
          k.clone(),
          Located {
            pos: v.pos,
            v: self.defer(v.pos, env, &v.rhs).await?,
          },
        );
      }
    }

    for item in &attrs.dyn_attrs {
      let name_val = vref(self.eval(dynamic_env, &item.name).await?);
      if self.force(item.pos, &name_val).await?.as_null().is_some() {
        continue;
      }
      let name = Ident::from(&*self.force_string_no_context(item.pos, &name_val).await?);
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
        Located {
          pos: item.pos,
          v: self.defer(item.pos, dynamic_env, &item.value).await?,
        },
      );
    }

    Ok(Value::Attrs(Arc::new(new_attrs)))
  }

  #[async_recursion(?Send)]
  async fn coerce_to_string(
    &self,
    pos: Pos,
    v: &ValueRef,
    context: &mut PathSet,
    opts: CoerceOpts,
  ) -> Result<String> {
    Ok(match &*self.force(pos, v).await? {
      Value::String(Str { ctx: c1, s }) => {
        context.extend(c1.clone());
        s.clone()
      }
      Value::Path(p) => {
        let p = p
          .canonicalize()
          .with_context(|| format!("canonicalizing {}", p.display()))?;
        if opts.copy_to_store {
          self.copy_path_to_store(pos, &p, context).await?
        } else {
          p.display().to_string()
        }
      }
      Value::Attrs(a) => {
        if let Some(x) = a.get(&ident!("outPath")) {
          return self.coerce_to_string(pos, &x.v, context, opts).await;
        } else {
          throw!(pos, "cannot coerce an attrset to a string")
        }
      }
      Value::Bool(b) if opts.coerce_more => (if *b { "1" } else { "" }).into(),
      Value::Int(i) if opts.coerce_more => i.to_string(),
      Value::Float(f) if opts.coerce_more => f.to_string(),
      Value::Null if opts.coerce_more => "".into(),
      Value::List(ls) if opts.coerce_more => {
        let mut s = String::new();
        for (i, item) in ls.iter().enumerate() {
          if i > 0 {
            s.push(' ');
          }
          s.push_str(&self.coerce_to_string(pos, item, context, opts).await?);
        }
        s
      }
      v => throw!(pos, "cannot coerce {} to string", v.typename()),
    })
  }

  async fn coerce_new_string(&self, pos: Pos, v: &ValueRef, opts: CoerceOpts) -> Result<Str> {
    let mut ctx = PathSet::new();
    let s = self.coerce_to_string(pos, v, &mut ctx, opts).await?;
    Ok(Str { s, ctx })
  }

  async fn coerce_to_path(&self, pos: Pos, v: &ValueRef, context: &mut PathSet) -> Result<PathBuf> {
    let s = self
      .coerce_to_string(
        pos,
        v,
        context,
        CoerceOpts {
          copy_to_store: false,
          coerce_more: false,
        },
      )
      .await?;
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

  async fn clone_value(&self, pos: Pos, v: &ValueRef) -> Result<Value> {
    Ok(self.force(pos, v).await?.clone())
  }

  #[async_recursion(?Send)]
  async fn eq_values(&self, pos: Pos, v1: &ValueRef, v2: &ValueRef) -> Result<bool> {
    if Arc::ptr_eq(v1, v2) {
      return Ok(true);
    }

    let v1 = self.force(pos, v1).await?;
    let v2 = self.force(pos, v2).await?;

    if let Some(result) = numeric_op(
      &*v1,
      &*v2,
      |i1, i2| i1 == i2,
      |f1, f2| (f1 - f2).abs() < std::f64::EPSILON,
    ) {
      return Ok(result);
    }

    Ok(match (&*v1, &*v2) {
      (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
      (Value::String(s1), Value::String(s2)) => s1.s == s2.s,
      (Value::Path(p1), Value::Path(p2)) => p1 == p2,
      (Value::Null, Value::Null) => true,

      (Value::Attrs(a1), Value::Attrs(a2)) => {
        if Arc::ptr_eq(a1, a2) {
          return Ok(true);
        }

        if let Some(out1) = a1.get(&ident!("outPath")) {
          if let Some(out2) = a2.get(&ident!("outPath")) {
            return self.eq_values(pos, &out1.v, &out2.v).await;
          }
        }

        if a1.len() != a2.len() {
          return Ok(false);
        }

        for (k, v) in a1.iter() {
          if let Some(v2) = a2.get(k) {
            if !self.eq_values(pos, &v.v, &v2.v).await? {
              return Ok(false);
            }
          } else {
            return Ok(false);
          }
        }

        true
      }

      (Value::List(l1), Value::List(l2)) => {
        if l1.len() != l2.len() {
          false
        } else {
          for (i, v) in l1.iter().enumerate() {
            if !self.eq_values(pos, v, &l2[i]).await? {
              return Ok(false);
            }
          }

          true
        }
      }

      _ => false,
    })
  }

  async fn eval_bool(&self, pos: Pos, env: &Env, expr: &Expr) -> Result<bool> {
    match self.eval(env, expr).await? {
      Value::Bool(b) => Ok(b),
      v => throw!(pos, "expected bool, got `{}'", v.typename()),
    }
  }

  async fn eval_list(&self, pos: Pos, env: &Env, expr: &Expr) -> Result<Arc<Vec<ValueRef>>> {
    Ok(match self.eval(env, expr).await? {
      Value::List(a) => a,
      v => throw!(pos, "expected list, got `{}'", v.typename()),
    })
  }

  async fn eval_attrs(&self, pos: Pos, env: &Env, expr: &Expr) -> Result<Arc<Attrs>> {
    Ok(match self.eval(env, expr).await? {
      Value::Attrs(a) => a,
      v => throw!(pos, "expected attrset, got `{}'", v.typename()),
    })
  }

  #[async_recursion(?Send)]
  async fn call_function(&self, pos: Pos, fun: &ValueRef, arg: &ValueRef) -> Result<Value> {
    match &*self.force(pos, fun).await? {
      Value::Primop(op, args) => {
        let mut args = args.clone();
        args.push(arg.clone());
        if args.len() == op.arity as usize {
          (op.fun)(self, pos, args).await
        } else {
          Ok(Value::Primop(*op, args))
        }
      }
      Value::Attrs(aread) => {
        if let Some(functor) = aread.get(&ident!("__functor")) {
          let v2 = vref(self.call_function(pos, &functor.v, fun).await?);
          self.call_function(pos, &v2, arg).await
        } else {
          throw!(pos, "cannot call an attrset as a function")
        }
      }
      Value::Lambda(e, lam) => self.call_lambda(pos, e, lam, arg).await,
      v => throw!(pos, "cannot call a {} as a function", v.typename()),
    }
  }

  async fn call_lambda(&self, pos: Pos, env: &Env, lam: &Lambda, arg: &ValueRef) -> Result<Value> {
    let fn_env_ref = vref(Value::Blackhole);
    let new_env = env.cons(Scope::Dynamic(fn_env_ref.clone()));

    let mut fn_env = Attrs::new();

    match &lam.arg {
      LambdaArg::Formals { name, formals } => {
        let fs = self.force_attrs(pos, arg).await?;

        let mut attrs_used = 0;

        if let Some(n) = name {
          fn_env.insert(
            n.clone(),
            Located {
              pos,
              v: arg.clone(),
            },
          );
        }

        for formal in &formals.formals {
          if let Some(argval) = fs.get(&formal.name) {
            attrs_used += 1;
            fn_env.insert(formal.name.clone(), argval.clone());
          } else if let Some(ref x) = formal.def {
            let fallback = self.defer(pos, &new_env, x).await?;
            fn_env.insert(formal.name.clone(), Located::nowhere(fallback));
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
          for arg in fs.keys() {
            if !formals.formals.iter().any(|x| x.name == *arg) {
              throw!(
                pos,
                "{} called with unexpected argument `{}'",
                lam.name,
                arg
              );
            }
          }
          unreachable!()
        }
      }
      LambdaArg::Plain(n) => {
        fn_env.insert(
          n.clone(),
          Located {
            pos,
            v: arg.clone(),
          },
        );
      }
    }

    *fn_env_ref.write() = Value::Attrs(Arc::new(fn_env));

    self.eval(&new_env, &lam.body).await
  }

  async fn force<'v>(&self, pos: Pos, v: &'v ValueRef) -> Result<RwLockReadGuard<'v, Value>> {
    let guard = unwrap!(v.try_upgradable_read(), pos, "infinite recursion");
    if guard.needs_eval() {
      let mut guard2 =
        RwLockUpgradableReadGuard::try_upgrade(guard).map_err(|_| err!(pos, "deadlock"))?;
      if guard2.needs_eval() {
        let old_value = std::mem::replace(&mut *guard2, Value::Blackhole);
        let new_value = match old_value {
          Value::Apply(lhs, rhs) => self.call_function(pos, &lhs, &rhs).await?,
          Value::Thunk(Thunk(env, expr)) => self.eval(&env, &expr).await?,
          Value::Blackhole => throw!(pos, "infinite recursion"),
          _ => unreachable!("needs_eval() returned true, but this value does not need forcing"),
        };
        *guard2 = new_value;
      }
      Ok(RwLockWriteGuard::downgrade(guard2))
    } else {
      Ok(RwLockUpgradableReadGuard::downgrade(guard))
    }
  }

  async fn force_int(&self, pos: Pos, v: &ValueRef) -> Result<i64> {
    match &*self.force(pos, v).await? {
      Value::Int(i) => Ok(*i),
      v => throw!(pos, "expected int, got {}", v.typename()),
    }
  }

  async fn force_bool(&self, pos: Pos, v: &ValueRef) -> Result<bool> {
    match &*self.force(pos, v).await? {
      Value::Bool(b) => Ok(*b),
      v => throw!(pos, "expected bool, got {}", v.typename()),
    }
  }

  async fn expect_fn(&self, pos: Pos, v: &ValueRef) -> Result<()> {
    match &*self.force(pos, v).await? {
      Value::Lambda { .. } | Value::Primop { .. } => Ok(()),
      v => throw!(pos, "expected function, got {}", v.typename()),
    }
  }

  async fn force_attrs<'v>(
    &self,
    pos: Pos,
    v: &'v ValueRef,
  ) -> Result<MappedRwLockReadGuard<'v, Attrs>> {
    RwLockReadGuard::try_map(self.force(pos, v).await?, |m| m.as_attrs().map(|a| &**a))
      .map_err(|v| err!(pos, "expected attrset, got {}", v.typename()))
  }

  pub async fn force_list<'v>(
    &self,
    pos: Pos,
    v: &'v ValueRef,
  ) -> Result<MappedRwLockReadGuard<'v, [ValueRef]>> {
    RwLockReadGuard::try_map(self.force(pos, v).await?, |m| {
      m.as_list().map(|a| a.as_slice())
    })
    .map_err(|v| err!(pos, "expected list, got {}", v.typename()))
  }

  pub async fn force_string_no_context<'v>(
    &self,
    pos: Pos,
    v: &'v ValueRef,
  ) -> Result<MappedRwLockReadGuard<'v, str>> {
    RwLockReadGuard::try_map(self.force(pos, v).await?, |m| match m {
      Value::String(Str { s, ctx }) if ctx.is_empty() => Some(s.as_str()),
      _ => None,
    })
    .map_err(|v| {
      if v.as_string().is_some() {
        err!(pos, "string may not refer to a store path")
      } else {
        err!(pos, "expected string, got {}", v.typename())
      }
    })
  }

  pub async fn force_string<'v>(
    &self,
    pos: Pos,
    v: &'v ValueRef,
  ) -> Result<MappedRwLockReadGuard<'v, Str>> {
    RwLockReadGuard::try_map(self.force(pos, v).await?, |m| m.as_string())
      .map_err(|v| err!(pos, "expected string, got {}", v.typename()))
  }

  async fn copy_path_to_store<P: AsRef<Path>>(
    &self,
    pos: Pos,
    path: P,
    ctx: &mut PathSet,
  ) -> Result<String> {
    let path = path.as_ref();
    if path.ends_with(".drv") {
      throw!(pos, "filenames may not end in .drv");
    }

    let p = self
      .store
      .add_path_to_store(
        path.file_name().and_then(|x| x.to_str()).unwrap_or(""),
        path,
        FileIngestionMethod::Recursive,
        HashType::SHA256,
        &PathFilter::All,
        Repair::Off,
      )
      .await?;
    let realpath = self.store.print_store_path(&p);
    ctx.insert(realpath.clone());
    Ok(realpath)
  }
}

/// Try to apply a numeric operator to two values. If one value is `f64`, the
/// other is also cast to `f64`. If either value is not numeric, return `None`.
fn numeric_op<T, F: Fn(i64, i64) -> T, G: Fn(f64, f64) -> T>(
  v1: &Value,
  v2: &Value,
  ifun: F,
  ffun: G,
) -> Option<T> {
  match (v1, v2) {
    (Value::Int(i1), Value::Int(i2)) => Some(ifun(*i1, *i2)),
    (Value::Int(i1), Value::Float(f2)) => Some(ffun(*i1 as _, *f2)),
    (Value::Float(f1), Value::Int(i2)) => Some(ffun(*f1, *i2 as _)),
    (Value::Float(f1), Value::Float(f2)) => Some(ffun(*f1, *f2)),
    _ => None,
  }
}

fn pos_to_value(pos: Pos) -> Result<Value> {
  let fs = crate::util::FILES.lock();
  let fname: PathBuf = fs.name(pos.0).into();
  let loc = fs.location(pos.0, pos.1.start())?;
  let line = loc.line.to_usize();
  let col = loc.column.to_usize();
  drop(fs);

  Ok(Value::Attrs(Arc::new({
    let mut a = Attrs::new();
    a.insert(
      ident!("file"),
      Located {
        pos,
        v: vref(Value::string(fname.display().to_string())),
      },
    );
    a.insert(
      ident!("line"),
      Located {
        pos,
        v: vref(Value::Int(line as _)),
      },
    );
    a.insert(
      ident!("column"),
      Located {
        pos,
        v: vref(Value::Int(col as _)),
      },
    );
    a
  })))
}

#[cfg(test)]
mod tests {
  use super::{Eval, Value};
  use crate::util::*;

  impl Eval {
    pub(crate) async fn assert<I: AsRef<str>>(&self, input: I) -> Result<()> {
      let input = input.as_ref();
      let expr = self.eval_inline(input).await?;
      match expr {
        Value::Bool(b) => assert!(b, "assertion failed: {}", input),
        v => bail!(
          "assert_true expects a bool expression, but got {}",
          v.typename()
        ),
      }
      Ok(())
    }

    pub(crate) async fn assert_err<I: AsRef<str>>(&self, input: I) {
      let input = input.as_ref();
      assert!(
        self.eval_inline(input).await.is_err(),
        "expected evaluation to fail for:\n  {}",
        input
      )
    }
  }

  #[tokio::test]
  async fn test_eval() -> NixResult {
    let e = Eval::test();
    let expr = e
      .eval_inline("(import <nixpkgs> {}).stdenv.cc.outPath")
      .await?;
    e.print(&super::vref(expr)).await?;

    ok()
  }

  #[tokio::test]
  async fn test_recursive() -> NixResult {
    let e = Eval::test();

    e.assert("let a = { b = 1; }; inherit (a) b; in b == 1")
      .await?;

    ok()
  }

  #[tokio::test]
  async fn test_attrs_equality() -> NixResult {
    let e = Eval::test();

    e.assert("let x = { f = _: 1; }; in x == x").await?;
    e.assert("let x = { f = _: 1; }; in x != { inherit (x) f; }")
      .await?;
    e.assert("let x.f = { y = _: 1; }; in x == { inherit (x) f; }")
      .await?;

    e.assert("let x = { f = _: 1; }; in x != x // { inherit (x) f; }")
      .await?;
    e.assert("let x = { f = _: 1; }; in x == { inherit (x) f; } // x")
      .await?;
    e.assert(r#"let x = { f = _: 1; }; in x == builtins.removeAttrs (x // { g = 1; }) ["g"]"#)
      .await?;

    ok()
  }

  #[tokio::test]
  async fn test_map_attrs() -> NixResult {
    let e = Eval::test();

    e.assert("builtins.mapAttrs (x: y: y + 10) { x = 1; y = 2; } == { x = 11; y = 12; }")
      .await?;

    ok()
  }
}
