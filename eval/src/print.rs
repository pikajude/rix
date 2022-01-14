use super::*;
use std::collections::HashSet;
use termcolor::*;

impl Eval {
  pub async fn print(&self, value: &ValueRef) -> Result<()> {
    let mut printer = Printer::new(self, StandardStream::stderr(ColorChoice::Auto));
    printer.print(value).await
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

  #[async_recursion(?Send)]
  async fn print(&mut self, value: &ValueRef) -> Result<()> {
    if !self.seen.insert(Arc::as_ptr(value) as _) {
      self
        .writer
        .set_color(ColorSpec::default().set_fg(Some(Color::Black)))?;
      self.writer.write_all(b"...")?;
      self.writer.reset()?;
      return Ok(());
    }

    let real_value = self.eval.force(Pos::none(), value).await?;

    match &*real_value {
      Value::Null => {
        self
          .writer
          .set_color(ColorSpec::default().set_fg(Some(Color::Cyan)))?;
        self.writer.write_all(b"null")?;
        self.writer.reset()?;
      }
      Value::Bool(b) => {
        self
          .writer
          .set_color(ColorSpec::default().set_fg(Some(Color::Cyan)))?;
        write!(self.writer, "{}", b)?;
        self.writer.reset()?;
      }
      Value::Int(i) => {
        self
          .writer
          .set_color(ColorSpec::default().set_fg(Some(Color::Cyan)))?;
        write!(self.writer, "{}", i)?;
        self.writer.reset()?;
      }
      Value::List(l) => {
        write!(self.writer, "[ ")?;
        for i in l.iter() {
          self.print(i).await?;
          write!(self.writer, " ")?;
        }
        write!(self.writer, "]")?;
      }
      Value::Attrs(a) => {
        write!(self.writer, "{{ ")?;
        for (key, value) in a.iter() {
          write!(self.writer, "{} = ", key,)?;
          self.print(&value.v).await?;
          write!(self.writer, "; ")?;
        }
        write!(self.writer, "}}")?
      }
      Value::String(s) => write!(self.writer, "{:?}", s.s)?,
      Value::Path(p) => {
        self
          .writer
          .set_color(ColorSpec::default().set_fg(Some(Color::Green)))?;
        write!(self.writer, "{}", p.display())?;
        self.writer.reset()?;
      }
      Value::Lambda(_, l) => {
        self
          .writer
          .set_color(ColorSpec::default().set_fg(Some(Color::Blue)))?;
        write!(self.writer, "«lambda @ {}»", l.pos)?;
        self.writer.reset()?;
      }
      v => todo!("{}", v.typename()),
    }

    Ok(())
  }
}
