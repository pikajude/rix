use anyhow::*;
use libarchive3_sys::ffi;
use nix::NixPath;
use std::ffi::CStr;
use std::mem::MaybeUninit;
use std::path::Path;

pub fn unpack<P: AsRef<Path>, P1: AsRef<Path>>(path: P, dest: P1) -> Result<()> {
  let path = path.as_ref();
  let dest = dest.as_ref();

  if let Some(d) = dest.parent() {
    std::fs::create_dir_all(d)?;
  } else {
    bail!("trying to unpack {} into root directory", path.display());
  }

  unsafe {
    let archive = ffi::archive_read_new();

    macro_rules! check {
      ($e:expr, $msg:literal $(,)?) => {
        let status = $e;
        if status == ffi::ARCHIVE_EOF {
          bail!("reached end-of-file while unpacking archive");
        } else if status != ffi::ARCHIVE_OK {
          let reason = CStr::from_ptr(ffi::archive_error_string(archive));
          bail!($msg, reason.to_string_lossy());
        }
      };

      ($e:expr $(,)?) => {
        check!($e, "failed to extract archive: {}")
      };
    }

    ffi::archive_read_support_filter_all(archive);
    ffi::archive_read_support_format_all(archive);

    check!(path.with_nix_path(|p| ffi::archive_read_open_filename(archive, p.as_ptr(), 16384))?);

    let flags = ffi::ARCHIVE_EXTRACT_FFLAGS
      | ffi::ARCHIVE_EXTRACT_PERM
      | ffi::ARCHIVE_EXTRACT_TIME
      | ffi::ARCHIVE_EXTRACT_SECURE_SYMLINKS
      | ffi::ARCHIVE_EXTRACT_SECURE_NODOTDOT;

    loop {
      let mut entry = MaybeUninit::<*mut ffi::Struct_archive_entry>::uninit();
      let stat = ffi::archive_read_next_header(archive, entry.as_mut_ptr());
      if stat == ffi::ARCHIVE_EOF {
        break;
      } else if stat == ffi::ARCHIVE_WARN {
        crate::util::warn!(
          "{}",
          CStr::from_ptr(ffi::archive_error_string(archive)).to_string_lossy()
        );
      } else {
        check!(stat);
      }

      let entry = entry.assume_init();
      let entry_file = CStr::from_ptr(ffi::archive_entry_pathname(entry)).to_string_lossy();
      let fullpath = dest.join(&*entry_file);

      fullpath.with_nix_path(|p| ffi::archive_entry_set_pathname(entry, p.as_ptr()))?;

      check!(ffi::archive_read_extract(archive, entry, flags));
    }

    check!(
      ffi::archive_read_close(archive),
      "failed to close archive: {}"
    );
  }

  Ok(())
}
