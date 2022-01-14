use rix_util::*;

#[derive(Debug, Error)]
pub enum Catchable {
  #[error("assertion failed")]
  Assert(Pos),
  #[error("evaluation aborted with message: `{}'", _1)]
  Throw(Pos, String),
}

impl LocatedError for Catchable {
  fn diagnose(&self) -> Diagnostic<FileId> {
    let pos = match self {
      Self::Assert(p) => p,
      Self::Throw(p, _) => p,
    };
    Diagnostic::error()
      .with_labels(vec![Label::primary(pos.0, pos.1)])
      .with_message(self.to_string())
  }
}
