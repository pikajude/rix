use super::*;
use crossbeam::atomic::AtomicCell;
use regex::Regex;
use rix_store::DrvName;
use serde_json::{Serializer, Value as JSON};
use std::{
  borrow::Cow,
  cmp::Ordering,
  collections::{BTreeSet, HashMap},
  convert::TryInto,
};

// FIXME: upstream nix throws an exception in the CompareValues comparator if
// the two items are incomparable. PartialOrd doesn't give us any way to do the
// same (we would have to panic/catch instead), and we rely on the Ord
// constraint in order to build a BTreeSet for genericClosure.
//
// You must check the value of COMPARE_INVALID whenever you add to a set
// containing 'CompareValue's. Not doing so will cause Cthulhu to awaken.
static COMPARE_INVALID: AtomicCell<Option<(&'static str, &'static str)>> = AtomicCell::new(None);

struct CompareValues<'a>(Cow<'a, Value>);

impl PartialEq for CompareValues<'_> {
  fn eq(&self, other: &Self) -> bool {
    if let Some(result) = super::numbers(&*self.0, &*other.0, |i1, i2| i1 == i2, |f1, f2| f1 == f2)
    {
      result
    } else {
      match (&*self.0, &*other.0) {
        (Value::String(Str { s: s1, .. }), Value::String(Str { s: s2, .. })) => s1 == s2,
        (Value::Path(p1), Value::Path(p2)) => p1 == p2,
        _ => false,
      }
    }
  }
}

impl Eq for CompareValues<'_> {}

impl PartialOrd for CompareValues<'_> {
  fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
    if let Some(result) = numbers(
      &self.0,
      &other.0,
      |i1, i2| i1.partial_cmp(&i2),
      |f1, f2| f1.partial_cmp(&f2),
    ) {
      result
    } else {
      match (&*self.0, &*other.0) {
        (Value::String(Str { s: s1, .. }), Value::String(Str { s: s2, .. })) => s1.partial_cmp(s2),
        (Value::Path(p1), Value::Path(p2)) => p1.partial_cmp(p2),
        _ => None,
      }
    }
  }
}

impl Ord for CompareValues<'_> {
  fn cmp(&self, other: &Self) -> Ordering {
    if let Some(c) = self.partial_cmp(other) {
      c
    } else {
      COMPARE_INVALID.store(Some((self.0.typename(), other.0.typename())));
      Ordering::Equal
    }
  }
}

macro_rules! checktype {
  ($($t:tt)+) => {
    |eval, pos, args| {
      Ok(Value::Bool(matches!(&*eval.force(pos, &args[0])?, $($t)+)))
    }
  }
}

pub struct Init {
  globals: Attrs,
  builtins: Attrs,
}

impl Init {
  pub fn new() -> Self {
    Self {
      globals: Attrs::new(),
      builtins: Attrs::new(),
    }
  }

  pub fn add_constant(&mut self, name: &str, value: Value) {
    let value = Located::nowhere(vref(value));
    self.globals.insert(name.into(), value.clone());
    self
      .builtins
      .insert(name.strip_prefix("__").unwrap_or(name).into(), value);
  }

  pub fn add_primop(&mut self, name: &'static str, arity: u8, fun: PrimopFn) {
    let name2 = name.strip_prefix("__").unwrap_or(name);
    let v = Value::Primop(
      Primop {
        fun,
        name: name2,
        arity,
      },
      PrimopArgs::default(),
    );
    self.add_constant(name, v);
  }

  pub fn init<S: Store + ?Sized>(mut self, store: &S) -> Attrs {
    self.add_constant("true", Value::Bool(true));
    self.add_constant("false", Value::Bool(false));
    self.add_constant("null", Value::Null);

    self.add_primop("__sub", 2, prim_subtract);
    self.add_primop("__lessThan", 2, prim_less_than);

    self.add_constant("__currentSystem", Value::string("x86_64-linux"));
    self.add_constant("__nixPath", mk_nix_path());
    self.add_constant("__nixVersion", Value::string("2.3.9"));
    self.add_constant(
      "__storeDir",
      Value::string(store.store_path().display().to_string()),
    );

    // TODO: this should just be an `eval_file` but getting the thunk ordering right
    // is sort of a pain. on the plus side it only has to be forced once
    self.add_constant(
      "derivation",
      Value::Apply(
        vref(Value::Primop(
          Primop {
            fun: prim_import,
            name: "import",
            arity: 1,
          },
          PrimopArgs::default(),
        )),
        vref(Value::Path(PathBuf::from(concat!(
          env!("CARGO_MANIFEST_DIR"),
          "/corepkgs/derivation.nix"
        )))),
      ),
    );
    self.add_primop(
      "derivationStrict",
      1,
      super::derivation::prim_derivation_strict,
    );

    self.add_primop("__addErrorContext", 2, prim_add_error_context);
    self.add_primop("__compareVersions", 2, prim_compare_versions);
    self.add_primop("baseNameOf", 1, prim_base_name_of);
    self.add_primop("__findFile", 2, prim_find_file);
    self.add_primop("__fromJSON", 1, prim_from_json);
    self.add_primop("__functionArgs", 1, prim_function_args);
    self.add_primop("__genericClosure", 1, prim_generic_closure);
    self.add_primop("__getEnv", 1, prim_getenv);
    self.add_primop("import", 1, prim_import);
    self.add_primop("__parseDrvName", 1, prim_parse_drv_name);
    self.add_primop("__pathExists", 1, prim_path_exists);
    self.add_primop("placeholder", 1, prim_placeholder);
    self.add_primop("__readFile", 1, prim_read_file);
    self.add_primop("scopedImport", 2, prim_scoped_import);
    self.add_primop("__seq", 2, prim_seq);
    self.add_primop("__toJSON", 1, prim_to_json);
    self.add_primop("__tryEval", 1, prim_try_eval);

    self.add_primop("fetchTarball", 1, |eval, pos, args| {
      fetch::fetch(eval, pos, args, "fetchTarball", true, "source".into())
    });

    self.add_primop("__attrNames", 1, prim_attrnames);
    self.add_primop("__getAttr", 2, prim_get_attr);
    self.add_primop("__hasAttr", 2, prim_has_attr);
    self.add_primop("__intersectAttrs", 2, prim_intersect_attrs);
    self.add_primop("__listToAttrs", 1, prim_list_to_attrs);
    self.add_primop("removeAttrs", 2, prim_remove_attrs);
    self.add_primop("__unsafeGetAttrPos", 2, prim_get_attr_pos);

    self.add_primop("__concatLists", 1, prim_concat_lists);
    self.add_primop("__elem", 2, prim_elem);
    self.add_primop("__elemAt", 2, prim_elem_at);
    self.add_primop("__filter", 2, prim_filter);
    self.add_primop("__foldl'", 3, prim_foldl_strict);
    self.add_primop("__genList", 2, prim_gen_list);
    self.add_primop("__head", 1, prim_head);
    self.add_primop("__length", 1, |eval, pos, args| {
      Ok(Value::Int(eval.force_list(pos, &args[0])?.len() as i64))
    });
    self.add_primop("map", 2, prim_map);
    self.add_primop("__tail", 1, prim_tail);

    self.add_primop("__concatStringsSep", 2, prim_concat_strings_sep);
    self.add_primop("__match", 2, prim_match);
    self.add_primop("__replaceStrings", 3, prim_replace_strings);
    self.add_primop("__split", 2, prim_split);
    self.add_primop("__stringLength", 1, |eval, pos, args| {
      Ok(Value::Int(
        eval
          .coerce_new_string(pos, &args[0], CoerceOpts::default())?
          .s
          .len() as i64,
      ))
    });
    self.add_primop("__substring", 3, prim_substring);
    self.add_primop("toString", 1, prim_to_string);
    self.add_primop("__unsafeDiscardStringContext", 1, |eval, pos, args| {
      let s = eval.coerce_new_string(pos, &args[0], CoerceOpts::default())?;
      Ok(Value::string(s.s))
    });

    self.add_primop("__isAttrs", 1, checktype!(Value::Attrs { .. }));
    self.add_primop("__isBool", 1, checktype!(Value::Bool { .. }));
    self.add_primop(
      "__isFunction",
      1,
      checktype!(Value::Lambda { .. } | Value::Primop { .. }),
    );
    self.add_primop("__isList", 1, checktype!(Value::List { .. }));
    self.add_primop("isNull", 1, checktype!(Value::Null));
    self.add_primop("__isString", 1, checktype!(Value::String { .. }));

    self.finish()
  }

  fn finish(mut self) -> Attrs {
    let builtins_ref = vref(Value::Blackhole);
    self
      .builtins
      .insert("builtins".into(), Located::nowhere(builtins_ref.clone()));
    *builtins_ref.write() = Value::Attrs(Arc::new(self.builtins));
    self
      .globals
      .insert("builtins".into(), Located::nowhere(builtins_ref));
    self.globals
  }
}

fn prim_compare_versions(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  Ok(Value::Int(
    match do_compare(
      &*eval.force_string_no_context(pos, &args[0])?,
      &*eval.force_string_no_context(pos, &args[1])?,
    ) {
      Ordering::Less => -1,
      Ordering::Equal => 0,
      Ordering::Greater => 1,
    },
  ))
}

fn do_compare(s1: &str, s2: &str) -> Ordering {
  let mut iter1 = s1.split(|p| p == '.' || p == '-');
  let mut iter2 = s2.split(|p| p == '.' || p == '-');
  loop {
    let num1 = iter1.next();
    let num2 = iter2.next();
    if num1.is_none() && num2.is_none() {
      break Ordering::Equal;
    }
    let c1 = num1.unwrap_or("");
    let c2 = num2.unwrap_or("");
    if components_lt(c1, c2) {
      break Ordering::Less;
    } else if components_lt(c2, c1) {
      break Ordering::Greater;
    }
  }
}

fn components_lt(s1: &str, s2: &str) -> bool {
  let num1 = s1.parse::<i64>().ok();
  let num2 = s2.parse::<i64>().ok();
  if let (Some(n1), Some(n2)) = (num1, num2) {
    n1 < n2
  } else if s1.is_empty() && num2.is_some() || s1 == "pre" && s2 != "pre" {
    true
  } else if s2 == "pre" {
    false
  } else if num1.is_some() {
    true
  } else if num2.is_some() {
    false
  } else {
    s1 < s2
  }
}

fn prim_scoped_import(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let mut context = PathSet::new();
  let path = eval.coerce_to_path(pos, &args[1], &mut context)?;

  eval.store.realise_context(&context)?;

  let vscope = eval.force_attrs(pos, &args[0])?;

  if vscope.is_empty() {
    eval.eval_file(&path)
  } else {
    todo!("scopedImport is not implemented")
  }
}

fn prim_import(eval: &Eval, pos: Pos, mut args: PrimopArgs) -> Result<Value> {
  args.insert(0, vref(Value::Attrs(Default::default())));
  prim_scoped_import(eval, pos, args)
}

fn prim_remove_attrs(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let to_remove = eval.force_list(pos, &args[1])?;
  if to_remove.is_empty() {
    return eval.clone_value(pos, &args[0]);
  }

  let mut attrs = eval.force_attrs(pos, &args[0])?.clone();

  for val in to_remove.iter() {
    let attrname = eval.force_string_no_context(pos, val)?;
    attrs.remove(&Ident::from(&*attrname));
  }

  Ok(Value::Attrs(Arc::new(attrs)))
}

fn prim_get_attr_pos(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let attrname = eval.force_string_no_context(pos, &args[0])?;
  let attrs = eval.force_attrs(pos, &args[1])?;
  match attrs.get(&Ident::from(&*attrname)) {
    Some(l) => pos_to_value(l.pos),
    None => Ok(Value::Null),
  }
}

fn prim_gen_list(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let n = eval.force_int(pos, &args[1])?;
  if n < 0 {
    throw!(pos, "cannot create a list of size {}", n);
  }

  let mut new_list = Vec::with_capacity(n as usize);
  for ix in 0..n {
    let int_arg = vref(Value::Int(ix));
    new_list.push(vref(Value::Apply(args[0].clone(), int_arg)));
  }
  Ok(Value::List(Arc::new(new_list)))
}

fn prim_head(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let list = eval.force_list(pos, &args[0])?;
  if list.is_empty() {
    throw!(pos, "builtins.head: empty list");
  }
  eval.clone_value(pos, &list[0])
}

fn prim_tail(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let list = eval.force_list(pos, &args[0])?;
  if list.is_empty() {
    throw!(pos, "builtins.tail: empty list");
  }
  Ok(Value::List(Arc::new(list[1..].to_vec())))
}

fn prim_get_attr(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let attrname = eval.force_string_no_context(pos, &args[0])?;
  let attrs0 = eval.force_attrs(pos, &args[1])?;
  match attrs0.get(&Ident::from(&*attrname)) {
    Some(v) => eval.clone_value(pos, &v.v),
    None => throw!(pos, "attribute `{}' missing", attrname),
  }
}

fn prim_has_attr(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let attrname = eval.force_string_no_context(pos, &args[0])?;
  let attrs0 = eval.force_attrs(pos, &args[1])?;
  Ok(Value::Bool(attrs0.contains_key(&Ident::from(&*attrname))))
}

fn prim_list_to_attrs(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let list = eval.force_list(pos, &args[0])?;

  let mut new_attrs = Attrs::new();

  for item in list.iter() {
    let item = eval.force_attrs(pos, item)?;
    let name = item
      .get(&Ident::from("name"))
      .ok_or_else(|| err!(pos, "attribute `name' missing in a call to `listToAttrs'"))?;
    let name = Ident::from(&*eval.force_string_no_context(pos, &name.v)?);
    new_attrs.insert(
      name,
      item
        .get(&Ident::from("value"))
        .ok_or_else(|| err!(pos, "attribute `value' missing in a call to `listToAttrs'"))?
        .clone(),
    );
  }

  Ok(Value::Attrs(Arc::new(new_attrs)))
}

fn prim_map(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let items = eval.force_list(pos, &args[1])?;
  let mut new_items = Vec::with_capacity(items.len());
  for it in items.iter() {
    new_items.push(vref(Value::Apply(args[0].clone(), it.clone())));
  }
  Ok(Value::List(Arc::new(new_items)))
}

fn prim_attrnames(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let attrs = eval.force_attrs(pos, &args[0])?;
  let mut keys = Vec::with_capacity(attrs.len());

  for key in attrs.keys() {
    keys.push(vref(Value::string(key.to_string())));
  }

  Ok(Value::List(Arc::new(keys)))
}

fn prim_intersect_attrs(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let lhs = eval.force_attrs(pos, &args[0])?;
  let rhs = eval.force_attrs(pos, &args[1])?;

  let mut new_attrs = Attrs::new();

  for k in lhs.keys() {
    if let Some(v) = rhs.get(k) {
      new_attrs.insert(k.clone(), v.clone());
    }
  }

  Ok(Value::Attrs(Arc::new(new_attrs)))
}

fn prim_getenv(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let varname = eval.force_string_no_context(pos, &args[0])?;
  Ok(Value::string(std::env::var(&*varname).unwrap_or_default()))
}

fn prim_base_name_of(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let mut path = eval.coerce_new_string(
    pos,
    &args[0],
    CoerceOpts {
      copy_to_store: false,
      coerce_more: false,
    },
  )?;
  path.s = Path::new(&path.s)
    .file_name()
    .map_or_else(String::new, |x| x.to_string_lossy().to_string());
  Ok(Value::String(path))
}

fn prim_parse_drv_name(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let n = DrvName::new(&*eval.force_string_no_context(pos, &args[0])?);
  let mut attrs = Attrs::new();
  attrs.insert(
    Ident::from("name"),
    Located {
      pos,
      v: vref(Value::string(n.name)),
    },
  );
  attrs.insert(
    Ident::from("version"),
    Located {
      pos,
      v: vref(Value::string(n.version)),
    },
  );
  Ok(Value::Attrs(Arc::new(attrs)))
}

fn prim_path_exists(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let mut ctx = PathSet::new();
  let path = eval.coerce_to_path(pos, &args[0], &mut ctx)?;
  eval.store.realise_context(&ctx)?;

  Ok(Value::Bool(path.exists()))
}

fn prim_placeholder(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let out = eval.force_string_no_context(pos, &args[0])?;
  let place = Hash::placeholder(&*out);
  Ok(Value::string(place))
}

fn prim_read_file(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let mut ctx = PathSet::new();
  let path = eval.coerce_to_path(pos, &args[0], &mut ctx)?;
  eval.store.realise_context(&ctx)?;

  let contents =
    std::fs::read_to_string(&path).map_err(|e| LocatedStdError { pos, err: e.into() })?;
  if contents.contains('\x00') {
    throw!(pos, "file `{}' contains a null byte", path.display());
  }

  Ok(Value::string(contents))
}

fn prim_seq(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let _ = eval.force(pos, &args[0])?;
  eval.clone_value(pos, &args[1])
}

fn prim_generic_closure(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let attrs = eval.force_attrs(pos, &args[0])?;

  let s = attrs
    .get(&Ident::from("startSet"))
    .ok_or_else(|| err!(pos, "attribute `startSet` required"))?;

  let op = attrs
    .get(&Ident::from("operator"))
    .ok_or_else(|| err!(pos, "attribute `operator' required"))?;

  let start_set = eval.force_list(pos, &s.v)?;

  let mut work_set = vec![];

  for item in &*start_set {
    work_set.push(item.clone());
  }

  let mut res = vec![];
  let mut done_keys = BTreeSet::new();

  while let Some(ws) = work_set.pop() {
    let e = eval.force_attrs(pos, &ws)?.clone();
    if let Some(key) = e.get(&Ident::from("key")) {
      let key = eval.clone_value(key.pos, &key.v)?;
      if !done_keys.insert(CompareValues(Cow::Owned(key))) {
        continue;
      }
      res.push(ws.clone());
      let v = eval.call_function(pos, &op.v, &ws)?;
      let new_values = v
        .as_list()
        .ok_or_else(|| err!(pos, "`operator' should return a list, not {}", v.typename()))?;

      for item in new_values.iter() {
        work_set.push(item.clone());
      }
    }
  }

  Ok(Value::List(Arc::new(res)))
}

fn prim_function_args(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let arg0 = eval.force(pos, &args[0])?;
  let lam = arg0
    .as_lambda()
    .ok_or_else(|| err!(pos, "`functionArgs' requires a function"))?
    .1;

  match &lam.arg {
    LambdaArg::Plain(_) => Ok(Value::Attrs(Arc::new(Attrs::new()))),
    LambdaArg::Formals { formals, .. } => {
      let mut attrs = Attrs::new();
      for f in &formals.formals {
        attrs.insert(
          f.name.clone(),
          Located {
            v: vref(Value::Bool(f.def.is_some())),
            pos,
          },
        );
      }
      Ok(Value::Attrs(Arc::new(attrs)))
    }
  }
}

fn prim_add_error_context(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  match eval.clone_value(pos, &args[1]) {
    Ok(v) => Ok(v),
    Err(e) => {
      let new_context = eval.coerce_new_string(pos, &args[0], CoerceOpts::default())?;
      Err(e.context(new_context.s))
    }
  }
}

fn prim_subtract(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let v1 = eval.force(pos, &args[0])?;
  let v2 = eval.force(pos, &args[1])?;
  numbers(
    &*v1,
    &*v2,
    |i1, i2| Value::Int(i1 - i2),
    |f1, f2| Value::Float(f1 - f2),
  )
  .ok_or_else(|| {
    err!(
      pos,
      "cannot subtract {} from {}",
      v2.typename(),
      v1.typename()
    )
  })
}

fn prim_to_string(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  Ok(Value::String(eval.coerce_new_string(
    pos,
    &args[0],
    CoerceOpts {
      coerce_more: true,
      copy_to_store: false,
    },
  )?))
}

fn prim_less_than(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let v1 = eval.force(pos, &args[0])?;
  let v2 = eval.force(pos, &args[1])?;

  let v1 = CompareValues(Cow::Borrowed(&*v1));
  let v2 = CompareValues(Cow::Borrowed(&*v2));

  if let Some(res) = v1.partial_cmp(&v2) {
    Ok(Value::Bool(res == Ordering::Less))
  } else {
    throw!(
      pos,
      "cannot compare {} with {}",
      v1.0.typename(),
      v2.0.typename()
    )
  }
}

fn prim_concat_strings_sep(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let Str { s: sep, mut ctx } = eval.force_string(pos, &args[0])?.clone();

  let strs = eval.force_list(pos, &args[1])?;
  let mut new = String::new();

  for (i, item) in strs.iter().enumerate() {
    if i > 0 {
      new.push_str(&sep);
    }
    new.push_str(&eval.coerce_to_string(pos, item, &mut ctx, CoerceOpts::default())?);
  }

  Ok(Value::String(Str { s: new, ctx }))
}

fn prim_replace_strings(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let from = eval.force_list(pos, &args[0])?;
  let to = eval.force_list(pos, &args[1])?;
  if from.len() != to.len() {
    throw!(
      pos,
      "`from' and `to' arguments to `replaceStrings' have different lengths"
    );
  }

  let mut from_list = vec![];
  for f in from.iter() {
    from_list.push(eval.force_string(pos, f)?);
  }

  let mut to_list = vec![];
  for t in to.iter() {
    to_list.push(eval.force_string(pos, t)?);
  }

  let Str {
    s: haystack,
    mut ctx,
  } = eval.force_string(pos, &args[2])?.clone();

  let mut output = String::new();

  let mut p = 0;

  while p <= haystack.len() {
    let mut found = false;

    for (ix, from) in from_list.iter().enumerate() {
      let to = &to_list[ix];
      if haystack.starts_with(&from.s) {
        found = true;
        output.push_str(&to.s);
        if from.s.is_empty() {
          if p < haystack.len() {
            output.push(haystack.as_bytes()[p] as char);
          }
          p += 1;
        } else {
          p += from.s.len();
        }
        ctx.extend(to.ctx.iter().cloned());
        break;
      }
    }
    if !found {
      if p < haystack.len() {
        output.push(haystack.as_bytes()[p] as char);
      }
      p += 1;
    }
  }

  Ok(Value::String(Str { s: output, ctx }))
}

lazy_static! {
  static ref REGEX_CACHE: Mutex<HashMap<String, Regex>> = Default::default();
}

impl REGEX_CACHE {
  fn get(&self, regex: &str) -> Result<Regex> {
    {
      if let Some(r) = self.lock().get(regex) {
        return Ok(r.clone());
      }
    }

    let compiled = Regex::new(regex)?;
    self.lock().insert(regex.to_string(), compiled.clone());
    Ok(compiled)
  }
}

fn prim_match(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let mut regex_str = String::from("^");
  regex_str.push_str(&*eval.force_string_no_context(pos, &args[0])?);
  regex_str.push('$');
  let reg = REGEX_CACHE.get(&regex_str)?;
  let haystack = eval.force_string(pos, &args[1])?;

  if let Some(caps) = reg.captures(&haystack.s) {
    let strs = caps
      .iter()
      .skip(1)
      .map(|s| vref(s.map_or(Value::Null, |m| Value::string(m.as_str()))))
      .collect::<Vec<_>>();
    Ok(Value::List(Arc::new(strs)))
  } else {
    Ok(Value::Null)
  }
}

#[test]
fn test_match() -> NixResult {
  let eval = Eval::test();

  eval.assert(r#"builtins.match "ab" "abc" == null"#)?;
  eval.assert(r#"builtins.match "abc" "abc" == []"#)?;
  eval.assert(r#"builtins.match "a(b)(c)" "abc" == [ "b" "c" ]"#)?;
  eval.assert(
    r#"
    builtins.match "[[:space:]]+([[:upper:]]+)[[:space:]]+" "  FOO   "
      == [ "FOO" ]
      "#,
  )?;

  ok()
}

fn prim_split(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let regex = eval.force_string_no_context(pos, &args[0])?;
  let regex = REGEX_CACHE.get(&*regex)?;

  let haystack = eval.force_string(pos, &args[1])?;

  let mut caps_iter = regex.captures_iter(&haystack.s).peekable();

  if caps_iter.peek().is_none() {
    return Ok(Value::List(Arc::new(vec![args[1].clone()])));
  }

  let mut parts = vec![];
  let mut prev_end = 0usize;
  for cap in caps_iter {
    let mut cap_iter = cap.iter();
    let full_match = cap_iter
      .next()
      .unwrap()
      .expect("invariant: capture group 0 doesn't exist");
    parts.push(vref(Value::string(
      &haystack.s[prev_end..full_match.start()],
    )));
    prev_end = full_match.end();
    let mut match_elems = vec![];
    for m in cap_iter {
      if let Some(m) = m {
        match_elems.push(vref(Value::string(m.as_str())));
      } else {
        match_elems.push(vref(Value::Null));
      }
    }
    parts.push(vref(Value::List(Arc::new(match_elems))));
  }

  parts.push(vref(Value::string(&haystack.s[prev_end..])));
  Ok(Value::List(Arc::new(parts)))
}

#[test]
fn test_split() -> NixResult {
  let eval = Eval::test();

  eval.assert(r#"builtins.split "(a)b" "abc" == [ "" [ "a" ] "c" ]"#)?;
  eval.assert(r#"builtins.split "([ac])" "abc" == [ "" [ "a" ] "b" [ "c" ] "" ]"#)?;
  eval.assert(r#"builtins.split "(a)|(c)" "abc" == [ "" [ "a" null ] "b" [ null "c" ] "" ]"#)?;
  eval.assert(r#"builtins.split "([[:upper:]]+)" "  FOO  " == [ "  " [ "FOO" ] "  " ]"#)?;

  ok()
}

fn prim_substring(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let start = eval.force_int(pos, &args[0])?;
  let len = eval.force_int(pos, &args[1])?;
  let mut new_str = eval.coerce_new_string(pos, &args[2], CoerceOpts::default())?;

  let start: usize = start
    .try_into()
    .map_err(|_| err!(pos, "negative start position in `substring'"))?;

  if start >= new_str.s.len() {
    new_str.s.clear();
  } else {
    new_str.s = new_str.s.split_off(start);
    new_str.s.truncate(len as usize);
  }

  Ok(Value::String(new_str))
}

fn prim_concat_lists(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let items = eval.force_list(pos, &args[0])?;

  let mut new_list = vec![];

  for list in items.iter() {
    for item in eval.force_list(pos, list)?.iter() {
      new_list.push(item.clone());
    }
  }

  Ok(Value::List(Arc::new(new_list)))
}

fn prim_elem(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let haystack = eval.force_list(pos, &args[1])?;
  for item in haystack.iter() {
    if eval.eq_values(pos, &args[0], item)? {
      return Ok(Value::Bool(true));
    }
  }
  Ok(Value::Bool(false))
}

fn prim_elem_at(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let n = eval.force_int(pos, &args[1])? as usize;
  match eval.force_list(pos, &args[0])?.get(n) {
    Some(l) => eval.clone_value(pos, l),
    None => throw!(pos, "list index `{}' out of bounds", n),
  }
}

fn prim_filter(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  eval.expect_fn(pos, &args[0])?;
  let haystack = eval.force_list(pos, &args[1])?;

  let mut new_list = Vec::with_capacity(haystack.len());

  for item in haystack.iter() {
    let val = eval.call_function(pos, &args[0], item)?;
    if let Value::Bool(b) = val {
      if b {
        new_list.push(item.clone());
      }
    } else {
      throw!(
        pos,
        "`filter' predicate should return bool, not {}",
        val.typename()
      );
    }
  }

  Ok(Value::List(Arc::new(new_list)))
}

fn prim_foldl_strict(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  eval.expect_fn(pos, &args[0])?;
  let items = eval.force_list(pos, &args[2])?;

  let mut acc = &args[1];
  let mut full_op;
  for item in items.iter() {
    let partial_op = vref(eval.call_function(pos, &args[0], acc)?);
    full_op = vref(eval.call_function(pos, &partial_op, item)?);
    acc = &full_op;
  }

  eval.clone_value(pos, acc)
}

#[test]
fn test_foldl_strict() -> NixResult {
  let e = Eval::test();
  e.assert(
    r#"
    builtins.foldl' (x: y: "${x}-${y}") "a" ["b" "c" "d"]
      == "a-b-c-d"
  "#,
  )?;

  ok()
}

fn prim_try_eval(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  match eval.clone_value(pos, &args[0]) {
    Ok(v) => {
      let mut attrs = Attrs::new();
      attrs.insert(Ident::from("value"), Located { pos, v: vref(v) });
      attrs.insert(
        Ident::from("success"),
        Located {
          pos,
          v: vref(Value::Bool(true)),
        },
      );
      Ok(Value::Attrs(Arc::new(attrs)))
    }
    Err(e) => match e.downcast::<Catchable>() {
      Ok(_) => {
        let mut attrs = Attrs::new();
        attrs.insert(
          Ident::from("value"),
          Located {
            pos,
            v: vref(Value::Bool(false)),
          },
        );
        attrs.insert(
          Ident::from("success"),
          Located {
            pos,
            v: vref(Value::Bool(false)),
          },
        );
        Ok(Value::Attrs(Arc::new(attrs)))
      }
      Err(e) => Err(e),
    },
  }
}

fn prim_find_file(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let entries = eval.force_list(pos, &args[0])?;
  let target = eval.force_string_no_context(pos, &args[1])?;
  let mut path_parts = Path::new(&*target).components();
  let search_key = path_parts
    .next()
    .expect("there must be at least one item in a filepath")
    .as_os_str();
  let has_children = path_parts.as_path().iter().next().is_some();
  let add_children = move |p: PathBuf| {
    if has_children {
      p.join(path_parts.as_path())
    } else {
      p
    }
  };

  for entry in &*entries {
    let attrs = eval.force_attrs(pos, entry)?;
    let path = attrs
      .get(&Ident::from("path"))
      .ok_or_else(|| err!(pos, "attribute `path' missing"))?;
    let path = eval.coerce_new_string(
      pos,
      &path.v,
      CoerceOpts {
        copy_to_store: false,
        coerce_more: false,
      },
    )?;
    let prefix = attrs
      .get(&Ident::from("prefix"))
      .ok_or_else(|| err!(pos, "attribute `prefix' missing"))?;
    let prefix = eval.force_string_no_context(pos, &prefix.v)?;

    if search_key == &*prefix {
      let full = add_children(path.s.into());
      if full.exists() {
        return Ok(Value::Path(full));
      }
    } else if prefix.is_empty() {
      if let Ok(iter) = std::fs::read_dir(&*path.s) {
        for next_item in iter {
          let next_item = next_item?;
          if next_item.file_name() == search_key {
            return Ok(Value::Path(add_children(next_item.path())));
          }
        }
      }
    }
  }

  bail!(Catchable::Throw(
    pos,
    format!(
      "entry `{}' is not in the Nix search path (try adding it using -I or by setting $NIX_PATH)",
      target
    )
  ))
}

fn prim_to_json(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let mut v = Serializer::new(Vec::new());
  let mut ctx = PathSet::new();

  serialize(eval, &mut v, pos, &args[0], &mut ctx)?;

  Ok(Value::String(Str {
    s: unsafe { String::from_utf8_unchecked(v.into_inner()) },
    ctx,
  }))
}

fn prim_from_json(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let s = eval.force_string_no_context(pos, &args[0])?;
  json_to_value(eval, pos, serde_json::from_str(&*s)?)
}

fn serialize(
  eval: &Eval,
  ser: &mut Serializer<Vec<u8>>,
  pos: Pos,
  v: &ValueRef,
  ctx: &mut PathSet,
) -> Result<()> {
  use serde::ser::Serializer as _;

  match &*eval.force(pos, v)? {
    Value::Null => ser.serialize_none()?,
    Value::String(Str { s, ctx: ctx2 }) => {
      ctx.extend(ctx2.iter().cloned());
      ser.serialize_str(&*s)?;
    }
    v => unimplemented!("toJSON: {}", v.typename()),
  }

  Ok(())
}

fn json_to_value(eval: &Eval, pos: Pos, value: JSON) -> Result<Value> {
  match value {
    JSON::Null => Ok(Value::Null),
    JSON::Bool(b) => Ok(Value::Bool(b)),
    JSON::Number(n) => {
      if let Some(i) = n.as_i64() {
        Ok(Value::Int(i))
      } else if let Some(f) = n.as_f64() {
        Ok(Value::Float(f))
      } else {
        throw!(pos, "json number `{}' cannot be represented in Nix", n)
      }
    }
    JSON::String(s) => Ok(Value::string(s)),
    JSON::Array(items) => {
      let mut list = Vec::with_capacity(items.len());
      for item in items {
        list.push(vref(json_to_value(eval, pos, item)?));
      }
      Ok(Value::List(Arc::new(list)))
    }
    JSON::Object(obj) => {
      let mut attrs = Attrs::new();
      for (k, v) in obj {
        attrs.insert(
          Ident::from(k),
          Located {
            pos,
            v: vref(json_to_value(eval, pos, v)?),
          },
        );
      }
      Ok(Value::Attrs(Arc::new(attrs)))
    }
  }
}

fn mk_nix_path() -> Value {
  let mut entries = vec![];
  for entry in get_nix_path().into_iter().chain(std::iter::once(format!(
    "nix={}/corepkgs",
    env!("CARGO_MANIFEST_DIR")
  ))) {
    let mut parts = entry.splitn(2, '=');
    let first = parts.next().unwrap();
    let second = parts.next();
    let (prefix, path) = match second {
      Some(x) => (first, x),
      None => ("", first),
    };
    let mut entry_attr = Attrs::new();
    entry_attr.insert("path".into(), Located::nowhere(vref(Value::string(path))));
    entry_attr.insert(
      "prefix".into(),
      Located::nowhere(vref(Value::string(prefix))),
    );
    entries.push(vref(Value::Attrs(Arc::new(entry_attr))));
  }
  Value::List(Arc::new(entries))
}

fn parse_nix_path(n: &str) -> Vec<&str> {
  let mut strings = vec![];
  let mut start = 0;
  let mut prev_colon = 0;
  for (next_colon, _) in n.match_indices(':') {
    if let Some(x) = n[prev_colon..next_colon].rfind('=') {
      if is_uri(&n[prev_colon + x + 1..]) {
        prev_colon = next_colon;
        continue;
      }
    }
    strings.push(&n[start..next_colon]);
    start = next_colon + 1;
  }
  if start < n.len() {
    strings.push(&n[start..]);
  }
  strings
}

fn get_nix_path() -> Vec<String> {
  if let Ok(n) = std::env::var("NIX_PATH") {
    parse_nix_path(&n)
      .into_iter()
      .map(|x| x.to_string())
      .collect()
  } else {
    vec![]
  }
}

fn is_uri(s: &str) -> bool {
  [
    "http://",
    "https://",
    "file://",
    "channel:",
    "channel://",
    "git://",
    "s3://",
    "ssh://",
  ]
  .iter()
  .any(|x| s.starts_with(x))
}
