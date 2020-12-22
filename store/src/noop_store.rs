use super::*;
use slog_scope::*;

pub struct NoopStore(PathBuf);

impl NoopStore {
  pub fn new() -> Self {
    let d = dirs::data_dir()
      .expect("no data dir set")
      .join("rix/nostore");
    std::fs::create_dir_all(&d).expect("unable to create tmpdir");
    Self(d)
  }
}

impl Default for NoopStore {
  fn default() -> Self {
    Self::new()
  }
}

impl Store for NoopStore {
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
    _: &str,
    path: &Path,
    _: FileIngestionMethod,
    _: HashType,
    _: (),
    _: Repair,
  ) -> Result<StorePath> {
    warn!("NOOP: adding path {} to store", path.display());
    Ok(crate::path::DUMMY.clone())
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

  fn add_to_store(&self, path_info: ValidPathInfo, _: Box<dyn Read>, _: Repair) -> Result<()> {
    warn!("NOOP: add calculated path to store: {:?}", path_info);
    Ok(())
  }
}
