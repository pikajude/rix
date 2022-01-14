use async_trait::async_trait;
use nix::fcntl::{flock, FlockArg};
use nix::unistd::getpid;
use rix_store::*;
use rix_util::hash::HashResult;
use rix_util::rusqlite::Connection;
use rix_util::*;
use std::fs::{File, OpenOptions};
use std::io::Read;
use std::os::unix::prelude::AsRawFd;
use std::path::{Path, PathBuf};
use std::time::{Duration, SystemTime};

const QUERY_PATH_INFO: &str = "select id, hash, registrationTime, deriver, narSize, ultimate, \
                               sigs, ca from ValidPaths where path = ?";

const QUERY_REFS: &str =
  "select path from Refs join ValidPaths on reference = id where referrer = ?";

pub struct LocalStore {
  basedir: PathBuf,
  store: PathBuf,
  roots_file: PathBuf,
  roots_fd: File,
  db: Sqlite,
}

impl LocalStore {
  pub fn new() -> Result<Self> {
    let basedir = PathBuf::from("/rix");
    let storedir = basedir.join("store");
    let statedir = basedir.join("var/rix");
    let dbdir = statedir.join("db");
    let temprootsdir = statedir.join("temproots");

    std::fs::create_dir_all(&storedir)?;
    std::fs::create_dir_all(&dbdir)?;
    std::fs::create_dir_all(&temprootsdir)?;

    let db = Sqlite::open(dbdir.join("rix.sqlite"))?;

    db.lock().execute_batch(include_str!(concat!(
      env!("CARGO_MANIFEST_DIR"),
      "/src/local_store/schema.sql"
    )))?;

    let roots_file = temprootsdir.join(format!("{}", getpid()));

    let roots_fd = OpenOptions::new()
      .create(true)
      .truncate(true)
      .write(true)
      .open(&roots_file)?;

    Ok(Self {
      basedir,
      store: storedir,
      db,
      roots_file,
      roots_fd,
    })
  }
}

#[async_trait]
impl Store for LocalStore {
  fn store_path(&self) -> &Path {
    &self.store
  }

  fn compute_fs_closure(
    &self,
    path: &StorePath,
    paths: &mut StorePathSet,
    opts: ClosureOpts,
  ) -> Result<()> {
    debug!("getting FS closure of {}", path);
    if paths.contains(path) {
      return Ok(());
    }
    paths.insert(path.clone());
    let qpi = self
      .query_path_info(path)?
      .ok_or_else(|| anyhow!("invalid path {}'", self.print_store_path(path)))?;
    for r in &qpi.refs {
      self.compute_fs_closure(r, paths, opts)?;
    }

    if opts.include_outputs && path.is_derivation() {
      for (_, (_, maybe_out_path)) in self
        .try_read_derivation(path)?
        .outputs_and_opt_paths(self)?
      {
        if let Some(o) = maybe_out_path {
          if self.is_valid_path(&o)? {
            self.compute_fs_closure(&o, paths, opts)?;
          }
        }
      }
    }

    if opts.include_derivers {
      if let Some(d) = &qpi.deriver {
        if self.is_valid_path(d)? {
          self.compute_fs_closure(d, paths, opts)?;
        }
      }
    }

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

  fn try_read_derivation(&self, path: &StorePath) -> Result<Derivation> {
    self.read_derivation(path)
  }

  fn add_text_to_store(
    &self,
    name: &str,
    contents: &[u8],
    refs: &StorePathSet,
    _: Repair,
  ) -> Result<StorePath> {
    let hash = Hash::new(contents, HashType::SHA256);
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
      let (read_side, mut write_side) = pipe()?;

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
        block_on(self.add_dump_to_store(Box::new(read_side), name, method, hash_type, repair))?;
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

  async fn add_to_store(
    &self,
    path_info: ValidPathInfo,
    source: Box<dyn Read + Send>,
    _: Repair,
  ) -> Result<()> {
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

      let HashResult {
        hash: hash_result,
        len: hash_len,
      } = nar_hasher.finish().1;

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

      self.register_valid_path(path_info).await?;
    }

    Ok(())
  }

  async fn add_dump_to_store(
    &self,
    source: Box<dyn Read + Send>,
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

    let HashResult { hash, len: size } = hash_sink.finish().1;

    let dst_path = self.make_fixed_output_path(method, hash, name, &Default::default(), false)?;

    if !self.is_valid_path(&dst_path)? {
      let real_path = self.to_real_path(&dst_path);

      rm_rf::ensure_removed(&real_path)?;

      // can't use rename() with possibly different filesystems (e.g. tmpfs)
      std::fs::copy(&dump_to, &real_path)?;
      rm_rf::ensure_removed(&dump_to)?;
    }

    let mut info = ValidPathInfo::new(dst_path.clone(), hash);
    info.nar_size = Some(size);

    self.register_valid_path(info).await?;

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

  async fn register_valid_paths(&self, mut infos: Vec<ValidPathInfo>) -> Result<()> {
    const REGISTER_VALID: &str = "insert into ValidPaths (path, hash, registrationTime, deriver, \
                                  narSize, ultimate, sigs, ca) values (?, ?, ?, ?, ?, ?, ?, ?)";
    const UPDATE_VALID: &str =
      "update ValidPaths set narSize = ?, hash = ?, ultimate = ?, sigs = ?, ca = ? where id = ?";

    let mut conn = self.db.lock();
    let db = conn.transaction()?;

    for info in &mut infos {
      let path = self.print_store_path(&info.path);

      if let Some(current_id) = lookup_path_id(&db, &path)? {
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
        info.id = current_id;
      } else {
        db.execute(
          REGISTER_VALID,
          params![
            path,
            info.nar_hash.encode_with_type(Encoding::Base16),
            info.registration_time_sql(),
            info
              .deriver
              .as_ref()
              .map_or_else(String::new, |d| self.print_store_path(d)),
            info.nar_size.unwrap_or_default() as i64,
            info.ultimate,
            "",
            ""
          ],
        )?;
        info.id = db.last_insert_rowid();
      }
    }

    for info in infos {
      let referrer = info.id;
      for r in &info.refs {
        let reference = lookup_path_id(&db, &self.print_store_path(r))?.expect("bad ref");
        db.execute(
          "insert or replace into Refs (referrer, reference) values (?, ?)",
          params![referrer, reference],
        )?;
      }
    }

    db.commit()?;

    Ok(())
  }

  fn log_file_of(&self, path: &StorePath) -> PathBuf {
    let mut log_part0 = path.to_string();
    let log_part1 = log_part0.split_off(2);

    self
      .basedir
      .join("var/log/nix/drvs")
      .join(log_part0)
      .join(log_part1)
  }

  fn add_temp_root(&self, path: &StorePath) -> Result<()> {
    debug!("acquiring write lock on '{}'", self.roots_file.display());
    flock(self.roots_fd.as_raw_fd(), FlockArg::LockExclusive)?;

    std::fs::write(
      &self.roots_file,
      format!("{}\0", self.print_store_path(path)),
    )?;

    debug!(
      "downgrading to read lock on '{}'",
      self.roots_file.display()
    );
    flock(self.roots_fd.as_raw_fd(), FlockArg::LockShared)?;

    Ok(())
  }
}

fn lookup_path_id(conn: &Connection, path: &str) -> rix_util::rusqlite::Result<Option<i64>> {
  conn
    .query_row::<i64, _, _>(
      "select id from ValidPaths where path = ?",
      params![&path],
      |r| r.get("id"),
    )
    .optional()
}
