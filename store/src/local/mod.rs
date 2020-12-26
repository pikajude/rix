use super::*;
use slog_scope::*;
use std::{
  fs::File,
  time::{Duration, SystemTime},
};

const QUERY_PATH_INFO: &str = "select id, hash, registrationTime, deriver, narSize, ultimate, \
                               sigs, ca from ValidPaths where path = ?";

const QUERY_REFS: &str =
  "select path from Refs join ValidPaths on reference = id where referrer = ?";

pub struct LocalStore {
  store: PathBuf,
  db: Sqlite,
}

impl LocalStore {
  pub fn new() -> Result<Self> {
    let d = dirs::data_dir().expect("no data dir set");
    let basedir = d.join("rix");
    let storedir = basedir.join("store");
    let dbdir = basedir.join("db");

    std::fs::create_dir_all(&storedir)?;
    std::fs::create_dir_all(&dbdir)?;

    let db = Sqlite::open(dbdir.join("rix.sqlite"))?;

    db.lock().execute_batch(include_str!(concat!(
      env!("CARGO_MANIFEST_DIR"),
      "/schema.sql"
    )))?;

    Ok(Self {
      store: storedir,
      db,
    })
  }
}

impl Store for LocalStore {
  fn store_path(&self) -> &Path {
    &self.store
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

  fn add_to_store(&self, path_info: ValidPathInfo, source: Box<dyn Read>, _: Repair) -> Result<()> {
    if !self.is_valid_path(&path_info.path)? {
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

      self.register_valid_path(path_info)?;
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

    if !self.is_valid_path(&dst_path)? {
      let real_path = self.to_real_path(&dst_path);

      rm_rf::ensure_removed(&real_path)?;

      std::fs::rename(&dump_to, &real_path)?;
    }

    let mut info = ValidPathInfo::new(dst_path.clone(), hash);
    info.nar_size = Some(size);

    self.register_valid_path(info)?;

    Ok(dst_path)
  }

  fn query_path_info(&self, path: &StorePath) -> Result<Option<ValidPathInfo>> {
    let db = self.db.lock();

    let mut stmt = db.prepare(QUERY_PATH_INFO)?;

    let mut iter = stmt.query_and_then::<_, anyhow::Error, _, _>(
      params![self.print_store_path(path)],
      |row| {
        let hash = Hash::decode(row.get::<_, String>("hash")?).with_context(|| {
          format!(
            "path-info entry for `{}' is invalid",
            self.print_store_path(path)
          )
        })?;

        let mut path_info = ValidPathInfo::new(path.clone(), hash);

        path_info.id = row.get::<_, i64>("id")?;
        path_info.registration_time = Some(
          SystemTime::UNIX_EPOCH
            + Duration::from_secs(row.get::<_, i64>("registrationTime")? as u64),
        );

        let deriver_path = row.get::<_, String>("deriver")?;
        if !deriver_path.is_empty() {
          path_info.deriver = Some(self.parse_store_path(Path::new(&deriver_path))?);
        }

        path_info.nar_size = Some(row.get::<_, i64>("narSize")? as _);
        path_info.ultimate = row.get("ultimate")?;

        Ok(path_info)
      },
    )?;

    if let Some(mut info) = iter.next().transpose()? {
      let mut stmt = db.prepare(QUERY_REFS)?;

      for ref_ in stmt.query_and_then(params![info.id], |row| row.get::<_, String>("path"))? {
        info.refs.insert(self.parse_store_path(Path::new(&ref_?))?);
      }

      Ok(Some(info))
    } else {
      Ok(None)
    }
  }

  fn register_valid_paths(&self, infos: Vec<ValidPathInfo>) -> Result<()> {
    const REGISTER_VALID: &str = "insert into ValidPaths (path, hash, registrationTime, deriver, \
                                  narSize, ultimate, sigs, ca) values (?, ?, ?, ?, ?, ?, ?, ?)";
    const UPDATE_VALID: &str =
      "update ValidPaths set narSize = ?, hash = ?, ultimate = ?, sigs = ?, ca = ? where id = ?";

    let db = self.db.lock();

    for info in infos {
      let path = self.print_store_path(&info.path);

      if let Some(current_id) = db
        .query_row::<i64, _, _>(
          "select id from ValidPaths where path = ?",
          params![&path],
          |r| r.get("id"),
        )
        .optional()?
      {
        db.execute(
          UPDATE_VALID,
          params![
            info.nar_size.unwrap_or_default() as i64,
            info.nar_hash.encode_with_type(Encoding::Base16),
            info.ultimate,
            "",
            "",
            current_id
          ],
        )?;
      } else {
        db.execute(
          REGISTER_VALID,
          params![
            path,
            info.nar_hash.encode_with_type(Encoding::Base16),
            info.registration_time_sql(),
            info
              .deriver
              .map_or_else(String::new, |d| self.print_store_path(&d)),
            info.nar_size.unwrap_or_default() as i64,
            info.ultimate,
            "",
            ""
          ],
        )?;
      }
    }

    Ok(())
  }
}
