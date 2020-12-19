use super::*;
use rix_store::{
  derivation::{FixedOutputHash, HashModulo, Output},
  Derivation, FileIngestionMethod, Repair,
};
use serde_json::Value as JSON;
use std::collections::BTreeSet;

fn decode_context(s: &str) -> (&str, &str) {
  break_str(s, '!').map_or(("", s), |(x, y)| (y, x))
}

pub fn prim_derivation_strict(eval: &Eval, pos: Pos, args: PrimopArgs) -> Result<Value> {
  let drv_attrs = eval.force_attrs(pos, &args[0])?;

  let drv_name = drv_attrs
    .get(&Ident::from("name"))
    .ok_or_else(|| err!(pos, "required derivation attribute `name' missing"))?;

  let drv_name_pos = drv_name.pos;
  let drv_name = eval.force_string_no_context(pos, &drv_name.v)?;

  let mut json_object;

  if let Some(s) = drv_attrs.get(&Ident::from("__structuredAttrs")) {
    if eval.force_bool(pos, &s.v)? {
      json_object = JSON::Object(Default::default());
    }
  }

  let mut ignore_nulls = false;
  if let Some(s) = drv_attrs.get(&Ident::from("__ignoreNulls")) {
    ignore_nulls = eval.force_bool(pos, &s.v)?;
  }

  let mut drv = Derivation::default();
  drv.name = drv_name.to_string();

  let mut ctx = PathSet::new();

  let mut content_addressed = false;

  let mut output_hash = None;
  let mut output_hash_algo = None;
  let mut ingest_method = FileIngestionMethod::Flat;

  let mut outputs = BTreeSet::<String>::new();
  outputs.insert("out".into());

  for (key, Located { v: value, .. }) in drv_attrs.iter() {
    if key == "__ignoreNulls" {
      continue;
    }
    trace!("processing attribute `{}'", key);

    if ignore_nulls && eval.force(pos, value)?.as_null().is_some() {
      continue;
    }

    if key == "__contentAddressed" {
      content_addressed = eval.force_bool(pos, value)?;
    } else if key == "args" {
      for arg in eval.force_list(pos, value)?.iter() {
        drv.args.push(eval.coerce_to_string(
          drv_name_pos,
          arg,
          &mut ctx,
          CoerceOpts {
            coerce_more: true,
            ..Default::default()
          },
        )?);
      }
    } else {
      let strval = eval.coerce_to_string(
        drv_name_pos,
        value,
        &mut ctx,
        CoerceOpts {
          coerce_more: true,
          ..Default::default()
        },
      )?;

      if key == "builder" {
        drv.builder = PathBuf::from(&strval);
      } else if key == "system" {
        drv.platform = strval.clone();
      } else if key == "outputHash" {
        output_hash = Some(strval.clone());
      } else if key == "outputHashAlgo" {
        output_hash_algo = Some(strval.clone());
      } else if key == "outputHashMode" {
        if strval == "recursive" {
          ingest_method = FileIngestionMethod::Recursive;
        } else if strval == "flat" {
          ingest_method = FileIngestionMethod::Flat;
        } else {
          throw!(
            drv_name_pos,
            "invalid value `{}' for `outputHashMode'",
            strval
          );
        }
      } else if key == "outputs" {
        for out_name in strval.split_ascii_whitespace() {
          if out_name == "drv" {
            throw!(drv_name_pos, "invalid derivation output name `drv'");
          }
          if !outputs.insert(out_name.to_string()) {
            throw!(drv_name_pos, "duplicate output name `{}'", out_name);
          }
        }
      }

      drv.env.insert(key.to_string(), strval);
    }
  }

  for path in &ctx {
    if let Some(path) = path.strip_prefix('=') {
      let mut refs = BTreeSet::new();
      eval
        .store
        .compute_fs_closure(&eval.store.parse_store_path(Path::new(path))?, &mut refs)?;
      for r in refs {
        drv.input_sources.insert(r.clone());
        if r.is_derivation() {
          let output_names = eval
            .store
            .read_derivation(&r)?
            .outputs
            .keys()
            .cloned()
            .collect();
          drv.input_derivations.insert(r, output_names);
        }
      }
    } else if let Some(path) = path.strip_prefix('!') {
      let (first, second) = decode_context(path);
      drv.input_derivations.insert(
        eval.store.parse_store_path(Path::new(&first))?,
        std::iter::once(second.to_string()).collect(),
      );
    } else {
      drv
        .input_sources
        .insert(eval.store.parse_store_path(Path::new(path))?);
    }
  }

  if drv.builder.to_str() == Some("") {
    throw!(drv_name_pos, "required attribute `builder' missing");
  }

  if drv.platform.is_empty() {
    throw!(drv_name_pos, "required attribute `system' missing");
  }

  if drv.name.ends_with(".drv") {
    throw!(drv_name_pos, "derivation names may not end in `.drv'");
  }

  if let Some(h) = &output_hash {
    if outputs.len() != 1 || outputs.first().unwrap() != "out" {
      throw!(
        drv_name_pos,
        "multiple outputs are not supported in fixed-output derivations"
      );
    }
    let hash_ty = output_hash_algo.and_then(|x| x.parse::<HashType>().ok());
    let h = Hash::new_allow_empty(h, hash_ty)?;

    let out_path = eval.store.make_fixed_output_path(
      ingest_method,
      h,
      &*drv_name,
      &Default::default(),
      false,
    )?;

    drv
      .env
      .insert("out".into(), eval.store.print_store_path(&out_path));
    drv.outputs.insert(
      "out".into(),
      Output::Fixed(FixedOutputHash {
        method: ingest_method,
        hash: h,
      }),
    );
  } else if content_addressed {
    let ht = output_hash_algo
      .and_then(|x| x.parse::<HashType>().ok())
      .ok_or_else(|| err!(drv_name_pos, "outputHashAlgo must be specified"))?;
    for outname in outputs {
      drv.env.insert(outname.clone(), Hash::placeholder(&outname));
      drv
        .outputs
        .insert(outname, Output::Floating(ingest_method, ht));
    }
  } else {
    for outname in &outputs {
      drv.env.insert(outname.clone(), String::new());
      drv.outputs.insert(
        outname.clone(),
        Output::InputAddressed(rix_store::path::DUMMY.clone()),
      );
    }

    match eval.store.hash_derivation_modulo(&drv, true)? {
      HashModulo::Normal(h) => {
        for outname in outputs {
          let path = eval.store.make_output_path(&outname, h, &drv_name)?;
          drv
            .env
            .insert(outname.clone(), eval.store.print_store_path(&path));
          drv.outputs.insert(outname, Output::InputAddressed(path));
        }
      }
      HashModulo::FixedOutput(_) => unreachable!(),
      HashModulo::Unknown => {
        for outname in outputs {
          drv.outputs.insert(outname, Output::Deferred);
        }
      }
    }
  }

  let drv_path = eval.store.write_derivation(&drv, Repair::Off, false)?;
  let drv_path_str = eval.store.print_store_path(&drv_path);

  debug!("instantiated `{}' -> `{}'", &drv_name, &drv_path_str);

  let mut attrs = Attrs::new();
  attrs.insert(
    "drvPath".into(),
    Located {
      pos,
      v: vref(Value::String(Str {
        s: drv_path_str.clone(),
        ctx: std::iter::once(format!("={}", drv_path_str)).collect(),
      })),
    },
  );

  for (name, output) in &drv.outputs {
    let strval = match output.path(&*eval.store, &drv.name, name)? {
      Some(p) => eval.store.print_store_path(&p),
      None => todo!("generate downstream placeholder"),
    };
    let mut s = Str {
      s: strval,
      ctx: Default::default(),
    };
    s.ctx.insert(format!(
      "!{}!{}",
      name,
      eval.store.print_store_path(&drv_path)
    ));
    attrs.insert(
      Ident::from(&**name),
      Located {
        pos,
        v: vref(Value::String(s)),
      },
    );
  }

  Ok(Value::Attrs(Arc::new(attrs)))
}
