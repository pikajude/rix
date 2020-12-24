use super::*;
use rix_util::nar::PathFilter;
use slog_scope::*;
use std::fs::File;

pub struct TestStore(PathBuf);

impl TestStore {
  pub fn new() -> Self {
    let d = dirs::data_dir()
      .expect("no data dir set")
      .join("rix/nostore");
    std::fs::create_dir_all(&d).expect("unable to create tmpdir");
    Self(d)
  }
}

impl Default for TestStore {
  fn default() -> Self {
    Self::new()
  }
}

impl Store for TestStore {
  fn store_path(&self) -> &Path {
    &self.0
  }

  fn compute_fs_closure(&self, path: &StorePath, _: &mut StorePathSet) -> Result<()> {
    warn!(
      "NOOP: compute FS closure for path: {}",
      self.print_store_path(path)
    );
    Ok(())
  }

  fn read_derivation(&self, path: &StorePath) -> Result<Derivation> {
    let contents = std::fs::read_to_string(self.to_real_path(path))?;
    Derivation::parse(
      self,
      path
        .derivation_name()
        .expect("path is not a derivation")
        .to_string(),
      contents,
    )
  }

  fn read_invalid_derivation(&self, path: &StorePath) -> Result<Derivation> {
    self.read_derivation(path)
  }

  fn add_text_to_store(
    &self,
    name: &str,
    contents: &[u8],
    refs: &StorePathSet,
    _: Repair,
  ) -> Result<StorePath> {
    let hash = Hash::hash(contents, HashType::SHA256);
    let dest_path = self.make_text_path(name, hash, refs)?;

    let real_path = self.to_real_path(&dest_path);

    info!("writing contents to store path {}", real_path.display());
    std::fs::write(real_path, contents)?;

    Ok(dest_path)
  }

  fn add_path_to_store(
    &self,
    name: &str,
    path: &Path,
    method: FileIngestionMethod,
    hash_type: HashType,
    filter: &PathFilter,
    repair: Repair,
  ) -> Result<StorePath> {
    crossbeam::scope(|s| {
      let (read_side, mut write_side) = pipe::new()?;

      let hdl = s.spawn::<_, Result<()>>(move |_| {
        if method == FileIngestionMethod::Recursive {
          nar::dump_path(path, write_side, filter)?;
        } else {
          let mut file = File::open(path)?;
          std::io::copy(&mut file, &mut write_side)?;
        }
        Ok(())
      });

      let store_path =
        self.add_dump_to_store(Box::new(read_side), name, method, hash_type, repair)?;
      hdl.join().unwrap()?;
      Ok(store_path)
    })
    .unwrap()
  }

  fn realise_context(&self, paths: &PathSet) -> Result<()> {
    if !paths.is_empty() {
      warn!("NOOP: realizing context for store paths: {:?}", paths);
    }
    Ok(())
  }

  fn check_uri(&self, uri: &str) -> Result<()> {
    warn!("NOOP: check URI {} for validity", uri);
    Ok(())
  }

  fn is_valid_path(&self, path: &StorePath) -> bool {
    self.to_real_path(path).exists()
  }

  fn add_to_store(&self, path_info: ValidPathInfo, source: Box<dyn Read>, _: Repair) -> Result<()> {
    warn!("NOOP: add calculated path to store: {:?}", path_info);

    if !self.is_valid_path(&path_info.path) {
      let real_path = self.to_real_path(&path_info.path);

      rm_rf::ensure_removed(&real_path)?;

      let refers_to_self = path_info.refs.contains(&path_info.path);
      let mut nar_hasher = if refers_to_self {
        todo!("hash modulo sink")
      } else {
        HashSink::new(HashType::SHA256, std::io::sink())
      };
      let combined = tee_readwrite::TeeReader::new(source, &mut nar_hasher, false);

      nar::restore_path(&real_path, combined)?;

      let (_, hash_result, hash_len) = nar_hasher.finish();

      if hash_result != path_info.nar_hash {
        bail!(
          "hash mismatch while importing path `{}';\n  specified: {}\n  got:       {}",
          real_path.display(),
          path_info.nar_hash.encode_with_type(Encoding::Base32),
          hash_result.encode_with_type(Encoding::Base32)
        );
      }

      if hash_len != path_info.nar_size.unwrap_or(0) {
        bail!(
          "size mismatch while importing path `{}';\n  specified: {}\n  got:       {}",
          real_path.display(),
          path_info.nar_size.unwrap_or(0),
          hash_len
        );
      }
    }

    Ok(())
  }

  fn add_dump_to_store(
    &self,
    source: Box<dyn Read>,
    name: &str,
    method: FileIngestionMethod,
    algo: HashType,
    _: Repair,
  ) -> Result<StorePath> {
    let mut hash_sink = HashSink::new(algo, std::io::sink());
    let mut hashing_source = tee_readwrite::TeeReader::new(source, &mut hash_sink, false);

    let dump_root = tempfile::tempdir()?;
    let dump_to = dump_root.path().join("x");

    if method == FileIngestionMethod::Recursive {
      nar::restore_path(&dump_to, hashing_source)?
    } else {
      let mut new_file = File::create(&dump_to)?;
      std::io::copy(&mut hashing_source, &mut new_file)?;
    }

    let (_, hash, size) = hash_sink.finish();

    let dst_path = self.make_fixed_output_path(method, hash, name, &Default::default(), false)?;

    if !self.is_valid_path(&dst_path) {
      let real_path = self.to_real_path(&dst_path);

      rm_rf::ensure_removed(&real_path)?;

      std::fs::rename(&dump_to, &real_path)?;
    }

    let mut info = ValidPathInfo::new(dst_path.clone(), hash);
    info.nar_size = Some(size);

    warn!("register path: {:?}", info);

    Ok(dst_path)
  }
}
