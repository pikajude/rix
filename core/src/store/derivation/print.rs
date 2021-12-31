use crate::store::prelude::*;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::{self, Display};

impl Derivation {
  pub fn print<'drv, S: Store + ?Sized>(
    &'drv self,
    store: &'drv S,
    mask_outputs: bool,
    override_inputs: OverrideInputs,
  ) -> Print<'drv, S> {
    Print {
      store,
      drv: self,
      mask_outputs,
      override_inputs,
    }
  }
}

struct Unquote<T>(T);

impl<T: Display> Display for Unquote<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    write!(f, "\"{}\"", self.0)
  }
}

struct Quote<T>(T);

impl<T: Display> Display for Quote<T> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("\"")?;
    for ch in self.0.to_string().chars() {
      match ch {
        '\"' | '\\' => write!(f, "\\{}", ch)?,
        '\n' => write!(f, "\\n")?,
        '\r' => write!(f, "\\r")?,
        '\t' => write!(f, "\\t")?,
        _ => write!(f, "{}", ch)?,
      }
    }
    f.write_str("\"")
  }
}

pub struct Print<'drv, S: Store + ?Sized + 'drv> {
  store: &'drv S,
  drv: &'drv Derivation,
  mask_outputs: bool,
  override_inputs: OverrideInputs,
}

macro_rules! write_strs {
  ($fmt:expr, $items:expr) => {{
    $fmt.write_str("[")?;
    for (ix, item) in $items.iter().enumerate() {
      if ix > 0 {
        $fmt.write_str(",")?;
      }
      write!($fmt, "{}", Unquote(item))?;
    }
    $fmt.write_str("]")
  }};
}

impl<'drv, S: Store + ?Sized + 'drv> Display for Print<'drv, S> {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    f.write_str("Derive([")?;

    for (ix, (name, out)) in self.drv.outputs.iter().enumerate() {
      if ix > 0 {
        f.write_str(",")?;
      }
      write!(f, r#"({}"#, Unquote(name))?;
      match out {
        Output::InputAddressed(i) => {
          write!(
            f,
            r#",{},"","""#,
            path = Unquote(if self.mask_outputs {
              String::default()
            } else {
              self.store.print_store_path(i)
            })
          )?;
        }
        Output::Fixed(hash) => {
          write!(
            f,
            r#",{path},{method},{hash}"#,
            path = Unquote(if self.mask_outputs {
              String::default()
            } else {
              self.store.print_store_path(
                &hash
                  .store_path(self.store, &self.drv.name, name)
                  .expect("unable to produce output path"),
              )
            }),
            method = Unquote(hash.method_algo()),
            hash = Unquote(hash.hash.encode(Encoding::Base16))
          )?;
        }
        Output::Floating(method, ty) => {
          write!(
            f,
            r#","","{prefix}{ty}","""#,
            prefix = method.prefix(),
            ty = ty
          )?;
        }
        Output::Deferred => {
          f.write_str(r#","","","""#)?;
        }
      }
      write!(f, ")")?;
    }

    f.write_str("],[")?;

    if let Some(a) = &self.override_inputs {
      for (ix, (name, ins)) in a.iter().enumerate() {
        if ix > 0 {
          f.write_str(",")?;
        }
        write!(f, r#"({},"#, Unquote(name))?;
        write_strs!(f, ins)?;
        f.write_str(")")?;
      }
    } else {
      for (ix, (path, ins)) in self.drv.input_derivations.iter().enumerate() {
        if ix > 0 {
          f.write_str(",")?;
        }
        write!(f, r#"({},"#, Unquote(self.store.print_store_path(path)))?;
        write_strs!(f, ins)?;
        f.write_str(")")?;
      }
    }

    f.write_str("],")?;

    let ordered_paths = self
      .drv
      .input_sources
      .iter()
      .map(|x| self.store.print_store_path(x))
      .collect::<BTreeSet<_>>();

    write_strs!(f, ordered_paths)?;

    write!(
      f,
      ",{},{},[",
      Unquote(&self.drv.platform),
      Quote(self.drv.builder.display())
    )?;

    for (ix, arg) in self.drv.args.iter().enumerate() {
      if ix > 0 {
        f.write_str(",")?;
      }
      write!(f, "{}", Quote(arg))?;
    }
    f.write_str("],[")?;

    for (ix, (key, val)) in self.drv.env.iter().enumerate() {
      if ix > 0 {
        f.write_str(",")?;
      }
      write!(
        f,
        "({},{})",
        Quote(key),
        Quote(if self.mask_outputs && self.drv.outputs.contains_key(key) {
          ""
        } else {
          val
        })
      )?;
    }

    f.write_str("])")
  }
}

type OverrideInputs = Option<BTreeMap<String, BTreeSet<String>>>;
