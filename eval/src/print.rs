use super::*;
use std::collections::HashSet;
use termcolor::*;

impl Eval {
  pub fn print(&self, value: &ValueRef) -> Result<()> {
    let mut printer = Printer::new(self, StandardStream::stderr(ColorChoice::Auto));
    printer.print(value)
  }
}

struct Printer<'e, W: WriteColor> {
  eval: &'e Eval,
  seen: HashSet<*const ()>,
  writer: W,
}

impl<'e, W: WriteColor> Printer<'e, W> {
  fn new(eval: &'e Eval, writer: W) -> Self {
    Self {
      eval,
      writer,
      seen: HashSet::new(),
    }
  }

  fn print(&mut self, value: &ValueRef) -> Result<()> {
    if !self.seen.insert(Arc::as_ptr(value) as _) {
      self
        .writer
        .set_color(&ColorSpec::default().set_fg(Some(Color::Black)))?;
      self.writer.write_all(b"...")?;
      self.writer.reset()?;
      return Ok(());
    }

    let real_value = self.eval.force(Pos::none(), value)?;

    match &*real_value {
      Value::Null => {
        self
          .writer
          .set_color(&ColorSpec::default().set_fg(Some(Color::Cyan)))?;
        self.writer.write_all(b"null")?;
        self.writer.reset()?;
      }
      Value::Attrs(a) => write!(
        self.writer,
        "{{ {:?} }}",
        a.keys().map(|x| x.as_ref()).collect::<Vec<_>>()
      )?,
      v => todo!("{:?}", std::mem::discriminant(v)),
    }

    Ok(())
  }
}
