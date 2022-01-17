mod archive;
mod cache;

use crate::util::nar::restore_path;
use crate::util::*;
use crate::value::{PathSet, PrimopArgs, Str, Value as NixValue};
use crate::Eval;
use cache::Cache;
use curl::easy::{Easy, HttpVersion, WriteError};
use nix::sys::stat::{fchmodat, FchmodatFlags, Mode};
use rix_store::{FileIngestionMethod, Repair, Store, StorePath, ValidPathInfo};
use serde_json::{Map, Value};
use slog::{Drain, Level};
use std::borrow::Cow;
use std::collections::HashMap;
use std::fs::File;
use std::io::Write;
use std::os::unix::fs::MetadataExt;
use std::path::{Path, PathBuf};
use std::time::Duration;

type CacheItem = cache::CacheItem<Map<String, Value>>;

pub struct DownloadedFile {
  pub path: StorePath,
  pub etag: Option<String>,
  pub real_url: String,
}

pub struct DownloadedPath {
  pub path: StorePath,
  pub real_path: PathBuf,
}

#[derive(Serialize)]
struct FileInfo<'s> {
  etag: Option<&'s str>,
  url: &'s str,
}

#[derive(Serialize)]
struct TarballInfo<'s> {
  etag: Option<&'s str>,
  #[serde(rename = "lastModified")]
  last_modified: i64,
}

pub fn download_tarball<S: Store + ?Sized>(
  store: &S,
  url: &str,
  name: &str,
  immutable: bool,
) -> Result<(DownloadedPath, i64)> {
  let mut input_attrs = HashMap::new();
  input_attrs.insert("type", "tarball");
  input_attrs.insert("url", url);
  input_attrs.insert("name", name);

  let cache = Cache::get()?;
  let cached = cache.lookup_expired::<_, _, Map<String, Value>>(store, &input_attrs)?;

  if let Some(c) = &cached {
    if !c.expired {
      return Ok((
        DownloadedPath {
          real_path: store.to_real_path(&c.path),
          path: c.path.clone(),
        },
        get_int(&c.info, "lastModified")?,
      ));
    }
  }

  let tarball = download_file(store, url, name, immutable)?;

  let unpacked_path;
  let last_modified;

  if let Some(item) = cached
    .as_ref()
    .zip_with(tarball.etag.as_deref(), matched_etag)
    .flatten()
  {
    unpacked_path = item.path.clone();
    last_modified = get_int(&item.info, "lastModified")?;
  } else {
    let unpack_dest = tempfile::tempdir()?;
    archive::unpack(store.to_real_path(&tarball.path), unpack_dest.path())?;
    let mut dir_contents = std::fs::read_dir(&unpack_dest)?;
    let topdir = dir_contents
      .next()
      .ok_or_else(|| anyhow!("extracted tarball is empty"))??;
    if dir_contents.next().is_some() {
      bail!("multiple top-level members in tarball");
    }
    last_modified = topdir.path().metadata()?.mtime();
    unpacked_path = store.add_path_to_store(
      name,
      &topdir.path(),
      FileIngestionMethod::Recursive,
      HashType::SHA256,
      &PathFilter::All,
      Repair::Off,
    )?;
  }

  let path_info = TarballInfo {
    etag: tarball.etag.as_deref(),
    last_modified,
  };

  cache.insert(store, &input_attrs, &path_info, &unpacked_path, immutable)?;

  Ok((
    DownloadedPath {
      real_path: store.to_real_path(&unpacked_path),
      path: unpacked_path,
    },
    last_modified,
  ))
}

pub fn download_file<S: Store + ?Sized>(
  store: &S,
  url: &str,
  name: &str,
  immutable: bool,
) -> Result<DownloadedFile> {
  let mut input_attrs = HashMap::new();
  input_attrs.insert("type", "file");
  input_attrs.insert("url", url);
  input_attrs.insert("name", name);

  let cache = Cache::get()?;

  let cached = cache.lookup_expired::<_, _, Map<String, Value>>(store, &input_attrs)?;

  if let Some(c) = &cached {
    if !c.expired {
      return Ok(DownloadedFile {
        path: c.path.clone(),
        etag: Some(get_str(&c.info, "etag")?),
        real_url: get_str(&c.info, "url")?,
      });
    }
  }

  let mut request = RequestInfo {
    url: url.to_string(),
    ..Default::default()
  };

  if let Some(c) = &cached {
    request.expect_etag = Some(get_str(&c.info, "etag")?);
  }

  let mut download_result = curl(request)?;

  let inf = FileInfo {
    etag: download_result.etag.as_deref(),
    url: &download_result.real_uri,
  };

  let store_path;

  if download_result.cached {
    store_path = cached.unwrap().path;
  } else {
    let mut nar_sink = hash::Sink::new(HashType::SHA256, tempfile::tempfile()?);
    nar::dump_with_len(
      download_result.len,
      &mut download_result.contents,
      &mut nar_sink,
    )?;
    store_path = store.make_fixed_output_path(
      FileIngestionMethod::Flat,
      download_result.hash,
      name,
      &Default::default(),
      false,
    )?;
    let (nar_file, nar_hash, nar_size) = nar_sink.finish_reset()?;
    let mut path_info = ValidPathInfo::new(store_path.clone(), nar_hash);
    path_info.nar_size = Some(nar_size);

    store.add_to_store(path_info, Box::new(nar_file), Repair::Off)?;
  }

  cache.insert(store, &input_attrs, &inf, &store_path, immutable)?;

  if url != download_result.real_uri {
    input_attrs.insert("url", &download_result.real_uri);
    cache.insert(store, &input_attrs, &inf, &store_path, immutable)?;
  }

  Ok(DownloadedFile {
    path: store_path,
    etag: download_result.etag,
    real_url: download_result.real_uri,
  })
}

pub fn builtin_fetchurl(env: &HashMap<String, String>) -> Result<()> {
  debug!("executing builder:fetchurl"; "env" => ?env);
  let getenv = |x: &'static str| {
    env
      .get(x)
      .ok_or_else(|| anyhow!("attribute '{}' missing", x))
  };
  let out_path = getenv("out")?;
  let url = getenv("url")?.to_string();
  let unpack = env.get("unpack").map_or(false, |x| x == "1");
  let req = RequestInfo {
    url,
    headers: vec![],
    expect_etag: None,
    verify_tls: false,
  };
  let mut res = curl(req)?;
  if unpack {
    restore_path(out_path, res.contents)?
  } else {
    let mut out = std::fs::File::create(out_path)?;
    std::io::copy(&mut res.contents, &mut out)?;
  }

  if env.get("executable").map_or(false, |x| x == "1") {
    fchmodat(
      None,
      out_path.as_str(),
      Mode::from_bits_truncate(0o755),
      FchmodatFlags::FollowSymlink,
    )?;
  }

  Ok(())
}

fn get_str(m: &Map<String, Value>, key: &str) -> Result<String> {
  Ok(
    m.get(key)
      .and_then(|x| x.as_str())
      .ok_or_else(|| anyhow!("attribute `{}' missing from cache item", key))?
      .to_string(),
  )
}

fn get_int(m: &Map<String, Value>, key: &str) -> Result<i64> {
  m.get(key)
    .and_then(|x| x.as_i64())
    .ok_or_else(|| anyhow!("attribute `{}' missing from cache item", key))
}

fn matched_etag<'c>(item: &'c CacheItem, tag: &str) -> Option<&'c CacheItem> {
  if get_str(&item.info, "etag").map_or(false, |x| x == tag) {
    Some(item)
  } else {
    None
  }
}

#[derive(Debug)]
struct CurlDownload {
  cached: bool,
  etag: Option<String>,
  real_uri: String,
  contents: File,
  hash: Hash,
  len: usize,
}

#[derive(Default)]
struct RequestInfo {
  url: String,
  headers: Vec<(String, String)>,
  expect_etag: Option<String>,
  verify_tls: bool,
}

fn curl(mut req: RequestInfo) -> Result<CurlDownload> {
  let mut easy = Easy::new();

  if slog_scope::logger().is_enabled(Level::Trace) {
    easy.verbose(true)?;
    easy.debug_function(|_, bytes| trace!("curl: {:?}", String::from_utf8_lossy(bytes)))?;
  }

  easy.url(&req.url)?;
  easy.follow_location(true)?;
  easy.max_redirections(10)?;
  easy.signal(false)?;
  easy.useragent(&format!(
    "curl/{} Nix/2.3.7",
    curl::Version::get().version()
  ))?;
  easy.pipewait(true)?;
  easy.progress(true)?;
  easy.http_version(HttpVersion::V11)?;
  if let Some(etag) = req.expect_etag.take() {
    req.headers.push(("If-None-Match".into(), etag));
  }
  if !req.headers.is_empty() {
    let mut hdr = curl::easy::List::new();
    for (name, val) in req.headers.iter() {
      hdr.append(&format!("{}: {}", name, val))?;
    }
    easy.http_headers(hdr)?;
  }

  if req.verify_tls {
    debug!("verify TLS: find a nix CA file")
  } else {
    easy.ssl_verify_peer(false)?;
    easy.ssl_verify_host(false)?;
  }

  easy.low_speed_limit(1)?;
  easy.low_speed_time(Duration::from_secs(300))?;

  let mut etag = None;
  let mut output = HashSink::new(HashType::SHA256, tempfile::tempfile()?);
  let mut got_status200 = false;
  let curl_res = {
    let mut tx = easy.transfer();
    tx.write_function(|data| {
      trace!("read: {} bytes", data.len());
      output.write_all(data).map_err(|_| {
        // TODO: this is probably wrong? I don't intend to call unpause
        WriteError::Pause
      })?;
      Ok(data.len())
    })?;
    tx.header_function(|data| {
      let header_line = String::from_utf8_lossy(data);
      if header_line.starts_with("HTTP/1.1 200") {
        got_status200 = true;
      }
      trace!("header: {:?}", header_line);
      if let Some((name, val)) = header_line.trim_end().break_on(':') {
        if name.eq_ignore_ascii_case("ETag") {
          etag = Some(val.strip_prefix(' ').unwrap_or(val).to_string());
          // hack for GitHub, which returns 200 OK even if the etag matches
          if got_status200 && etag == req.expect_etag {
            return false;
          }
        }
      }
      true
    })?;
    tx.progress_function(|a, b, c, d| {
      trace!("progress: {},{},{},{}", a, b, c, d);
      true
    })?;
    tx.perform()
  };

  let mut status = easy.response_code()?;

  if let Err(e) = curl_res {
    if e.is_write_error() && etag == req.expect_etag {
      status = 304;
    } else {
      bail!(e)
    }
  }

  let real_uri = easy.effective_url()?.map_or(req.url, |x| x.to_string());
  let (contents, hash, len) = output.finish_reset()?;

  Ok(CurlDownload {
    cached: status == 304,
    etag,
    real_uri,
    contents,
    hash,
    len,
  })
}

pub fn fetch(
  eval: &Eval,
  pos: Pos,
  args: PrimopArgs,
  fetcher: &'static str,
  unpack: bool,
  mut name: String,
) -> Result<NixValue> {
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
  Ok(NixValue::String(s))
}

fn resolve(s: &str) -> Cow<str> {
  if let Some(s) = s.strip_prefix("channel:") {
    Cow::Owned(format!("https://nixos.org/channels/{}/nixexprs.tar.xz", s))
  } else {
    Cow::Borrowed(s)
  }
}
