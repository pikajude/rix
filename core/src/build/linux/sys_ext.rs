use libc::{c_int, c_void};
use nix::errno::Errno;
use nix::sched::CloneFlags;
use nix::unistd::Pid;
use nix::Result;

extern "C" fn clone_cb<F>(data: *mut c_void) -> c_int
where
  F: FnOnce() -> isize,
{
  let cb = unsafe { Box::from_raw(data as *mut F) };
  (*cb)() as c_int
}

// adapted from https://github.com/nix-rust/nix/issues/360#issuecomment-359274560
pub fn clone<F>(
  callback: F,
  stack: &mut [u8],
  flags: CloneFlags,
  signal: Option<c_int>,
) -> Result<Pid>
where
  F: FnOnce() -> isize,
{
  let cb = Box::new(callback);
  let cb_ptr = Box::into_raw(cb) as *mut c_void;
  let res = unsafe {
    let combined = flags.bits() | signal.unwrap_or(0);
    let ptr = stack.as_mut_ptr().add(stack.len());
    let ptr_aligned = ptr.sub(ptr as usize % 16);
    libc::clone(clone_cb::<F>, ptr_aligned as *mut c_void, combined, cb_ptr)
  };
  Errno::result(res).map(Pid::from_raw)
}
