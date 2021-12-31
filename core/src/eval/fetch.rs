use std::borrow::Cow;

use super::*;

pub fn fetch(
  eval: &Eval,
  pos: Pos,
  args: PrimopArgs,
  fetcher: &'static str,
  unpack: bool,
  mut name: String,
) -> Result<Value> {
  let mut url = <Option<String>>::None;
  let mut expected_hash = <Option<Hash>>::None;

  if let Some(a) = eval.force(pos, &args[0])?.as_attrs() {
    for (key, val) in a.iter() {
      if key == "url" {
        url = Some(eval.force_string_no_context(val.pos, &val.v)?.to_string());
      } else if key == "sha256" {
        expected_hash = Some(Hash::new_allow_empty(
          &*eval.force_string_no_context(val.pos, &val.v)?,
          Some(HashType::SHA256),
        )?);
      } else if key == "name" {
        name = eval.force_string_no_context(val.pos, &val.v)?.to_string();
      } else {
        throw!(val.pos, "unsupported argument `{}' to `{}'", key, fetcher);
      }
    }

    if url.is_none() {
      throw!(pos, "`url' argument required");
    }
  } else {
    url = Some(eval.force_string_no_context(pos, &args[0])?.to_string());
  }

  let url = url.unwrap();
  let url = resolve(&url);

  eval.store.check_uri(&url)?;

  if name.is_empty() {
    name = Path::new(&*url)
      .file_name()
      .unwrap_or_default()
      .to_string_lossy()
      .to_string();
  }

  warn!("check pure-eval");

  let store_path = if unpack {
    crate::fetch::download_tarball(&*eval.store, &url, &name, expected_hash.is_some())?
      .0
      .path
  } else {
    crate::fetch::download_file(&*eval.store, &url, &name, expected_hash.is_some())?.path
  };

  let real_path = eval.store.to_real_path(&store_path);

  if let Some(e) = expected_hash {
    let actual = if unpack {
      eval.store.get_path_info(&store_path)?.nar_hash
    } else {
      Hash::hash_file(&real_path, HashType::SHA256)?.0
    };
    if actual != e {
      bail!(
        "hash mismatch in file downloaded from {}:\n  specified: {}\n  got:       {}",
        url,
        e.encode_with_type(Encoding::Base32),
        actual.encode_with_type(Encoding::Base32)
      );
    }
  }

  let path_str = real_path.display().to_string();

  let mut s = Str {
    s: path_str.clone(),
    ctx: PathSet::new(),
  };
  s.ctx.insert(path_str);
  Ok(Value::String(s))
}

fn resolve(s: &str) -> Cow<str> {
  if let Some(s) = s.strip_prefix("channel:") {
    Cow::Owned(format!("https://nixos.org/channels/{}/nixexprs.tar.xz", s))
  } else {
    Cow::Borrowed(s)
  }
}
