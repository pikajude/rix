use std::cmp::Ordering;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::err;
use crate::eval::CoerceOpts;
use crate::util::error::Catchable;
use crate::util::*;

use super::value::*;
use super::Eval;

#[derive(Default)]
pub struct Init {
  globals: Attrs,
  builtins: Attrs,
}

impl Init {
  pub fn new() -> Self {
    Self::default()
  }

  pub fn add_constant(&mut self, name: &str, value: Value) {
    self.globals.insert(name, Pos::none(), value.clone());
    self
      .builtins
      .insert(name.strip_prefix("__").unwrap_or(name), Pos::none(), value);
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

  pub fn init(mut self) -> Attrs {
    self.add_constant("true", Value::bool(true));
    self.add_constant("false", Value::bool(false));
    self.add_constant("null", Value::null());

    self.add_primop("__compareVersions", 2, prim_compare_versions);

    self.add_constant("__nixPath", mk_nix_path());
    self.add_constant("__nixVersion", Value::string("2.3.9"));

    self.add_primop("__findFile", 2, prim_find_file);
    self.add_primop("import", 1, prim_import);

    self.finish()
  }

  fn finish(mut self) -> Attrs {
    let mut v = <Arc<Attrs>>::default();
    self
      .builtins
      .insert("builtins", Pos::none(), Value::Attrs(v.clone()));
    self
      .globals
      .insert("builtins", Pos::none(), Value::Attrs(v.clone()));
    unsafe {
      // move builtins into itself
      *Arc::get_mut_unchecked(&mut v) = self.builtins;
    }
    self.globals
  }
}

fn prim_find_file(eval: &Eval, pos: Pos, mut args: PrimopArgs) -> Result<Value> {
  let entries = eval.force_list(pos, *args.remove(0))?;
  let target = eval.force_string_no_context(pos, *args.remove(0))?;

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

  for entry in entries.iter() {
    let attrs = eval.force_attrs(pos, entry.clone())?;
    let path = attrs
      .get(&ident!("path"))
      .ok_or_else(|| err!(pos, "attribute `path' missing"))?;
    let path = eval.coerce_new_string(
      pos,
      path.value.clone(),
      CoerceOpts {
        copy_to_store: false,
        coerce_more: false,
      },
    )?;
    let prefix = attrs
      .get(&ident!("prefix"))
      .ok_or_else(|| err!(pos, "attribute `prefix' missing"))?;
    let prefix = eval.force_string_no_context(pos, prefix.value.clone())?;

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
      "entry `{}' is not in the Nix search path (try adding it using -I or by setting $NIX_PATH",
      target
    )
  ))
}

fn prim_import(eval: &Eval, pos: Pos, mut args: PrimopArgs) -> Result<Value> {
  args.insert(0, Box::new(Value::Attrs(Default::default())));
  prim_scoped_import(eval, pos, args)
}

fn prim_scoped_import(eval: &Eval, pos: Pos, mut args: PrimopArgs) -> Result<Value> {
  let mut context = PathSet::new();
  let path = eval.coerce_to_path(pos, *args.remove(1), &mut context)?;

  let attrs = eval.force_attrs(pos, *args.remove(0))?;

  if attrs.is_empty() {
    eval.eval_file(path)
  } else {
    todo!("scopedImport is not implemented")
  }
}

fn prim_compare_versions(eval: &Eval, pos: Pos, mut args: PrimopArgs) -> Result<Value> {
  let v1 = eval.force_string_no_context(pos, *args.remove(0))?;
  let v2 = eval.force_string_no_context(pos, *args.remove(0))?;
  Ok(Value::int(match do_compare(&v1, &v2) {
    Ordering::Less => -1,
    Ordering::Equal => 0,
    Ordering::Greater => 1,
  }))
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

fn mk_nix_path() -> Value {
  let mut entries = vec![];
  for entry in get_nix_path().into_iter().chain(std::iter::once(format!(
    "nix={}/src/eval/corepkgs",
    env!("CARGO_MANIFEST_DIR")
  ))) {
    let mut parts = entry.splitn(2, '=');
    let first = parts.next().unwrap();
    let second = parts.next();
    let (prefix, path) = match second {
      Some(x) => (first, x),
      None => ("", first),
    };
    let mut entry = Attrs::default();
    entry.insert("path", Pos::none(), Value::string(path));
    entry.insert("prefix", Pos::none(), Value::string(prefix));
    entries.push(Value::Attrs(Arc::new(entry)));
  }
  Value::List(Arc::new(entries))
}

fn parse_nix_path(n: &str) -> Vec<&str> {
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
