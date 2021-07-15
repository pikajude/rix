use super::UserError;
use crate::util::*;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fmt;
use std::fmt::Display;
use std::path::PathBuf;
use std::sync::Arc;

pub type ExprRef = Arc<Expr>;

type ParseOk = Result<(), Located<UserError>>;

#[derive(Debug, EnumAsInner, Clone, Serialize, Deserialize)]
pub enum Expr {
  Pos {
    pos: Pos,
  },
  Var {
    pos: Pos,
    name: Ident,
  },
  Int {
    n: i64,
  },
  Float {
    f: f64,
  },
  String {
    s: String,
  },
  Path {
    path: PathBuf,
  },
  Select {
    pos: Pos,
    lhs: ExprRef,
    def: Option<ExprRef>,
    path: AttrPath,
  },
  Lambda(Lambda),
  List(Vec<ExprRef>),
  Attrs(Attrs),
  Assert {
    pos: Pos,
    cond: ExprRef,
    body: ExprRef,
  },
  With {
    pos: Pos,
    env: ExprRef,
    body: ExprRef,
  },
  Let {
    attrs: Attrs,
    body: ExprRef,
  },
  ConcatStrings {
    pos: Pos,
    force_string: bool,
    parts: Vec<Expr>,
  },
  If {
    pos: Pos,
    cond: ExprRef,
    rhs1: ExprRef,
    rhs2: ExprRef,
  },
  Op {
    bin: Bin,
    pos: Pos,
    lhs: ExprRef,
    rhs: ExprRef,
  },
  Apply {
    pos: Pos,
    lhs: ExprRef,
    rhs: ExprRef,
  },
  HasAttr {
    lhs: ExprRef,
    path: AttrPath,
  },
  Not {
    pos: Pos,
    e: ExprRef,
  },
}

pub fn swap<A, B>(x: Located<(A, B)>) -> Located<(B, A)> {
  Located {
    pos: x.pos,
    v: (x.v.1, x.v.0),
  }
}

impl Expr {
  pub fn bin(operator: Bin, args: Located<(Self, Self)>) -> Self {
    Self::Op {
      bin: operator,
      pos: args.pos,
      lhs: Arc::new(args.v.0),
      rhs: Arc::new(args.v.1),
    }
  }

  pub fn app2(name: &str, args: Located<(Self, Self)>) -> Self {
    Self::apply(Located {
      pos: args.pos,
      v: (
        Self::apply(Located {
          pos: args.pos,
          v: (
            Self::Var {
              pos: args.pos,
              name: name.into(),
            },
            args.v.0,
          ),
        }),
        args.v.1,
      ),
    })
  }

  pub fn lt(args: Located<(Self, Self)>, invert: bool) -> Self {
    let inner = Self::apply(Located {
      pos: args.pos,
      v: (
        Self::apply(Located {
          pos: args.pos,
          v: (
            Self::Var {
              pos: args.pos,
              name: "__lessThan".into(),
            },
            args.v.0,
          ),
        }),
        args.v.1,
      ),
    });
    if invert {
      Self::Not {
        pos: args.pos,
        e: Arc::new(inner),
      }
    } else {
      inner
    }
  }

  pub fn apply(args: Located<(Self, Self)>) -> Self {
    Self::Apply {
      pos: args.pos,
      lhs: Arc::new(args.v.0),
      rhs: Arc::new(args.v.1),
    }
  }

  pub fn string<S: Into<String>>(s: S) -> Self {
    let s = s.into();
    Self::String { s }
  }

  pub fn path<P: Into<PathBuf>>(p: P) -> Self {
    let p = p.into();
    Self::Path { path: p }
  }
}

fn show_attrpath(f: &mut fmt::Formatter, path: &[Located<AttrName>]) -> fmt::Result {
  for (i, attr) in path.iter().enumerate() {
    if i > 0 {
      f.write_str(".")?;
    }
    match &attr.v {
      AttrName::Static(k) => k.fmt(f)?,
      AttrName::Dynamic(e) => write!(f, "\"${{{}}}\"", e)?,
    }
  }
  Ok(())
}

#[derive(Debug, Display, Copy, Clone, Ord, PartialOrd, Eq, PartialEq, Serialize, Deserialize)]
pub enum Bin {
  #[display(fmt = "==")]
  Eq,
  #[display(fmt = "!=")]
  Neq,
  #[display(fmt = "&&")]
  And,
  #[display(fmt = "||")]
  Or,
  #[display(fmt = "->")]
  Impl,
  #[display(fmt = "//")]
  Update,
  #[display(fmt = "++")]
  ConcatLists,
}

pub type AttrPath = Vec<Located<AttrName>>;
pub type AttrList = Vec<Located<AttrName>>;

#[derive(Debug, EnumAsInner, Clone, Serialize, Deserialize)]
pub enum AttrName {
  Static(Ident),
  Dynamic(ExprRef),
}

#[derive(Debug, Clone)]
pub struct Var {
  pub pos: Pos,
  pub name: Ident,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum LambdaArg {
  Plain(Ident),
  Formals {
    name: Option<Ident>,
    formals: Formals,
  },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Formals {
  pub formals: Vec<Formal>,
  pub ellipsis: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Formal {
  pub pos: Pos,
  pub name: Ident,
  pub def: Option<ExprRef>,
}

#[derive(Debug)]
pub enum ParseBinding {
  Plain(AttrPath, Expr),
  Inherit(AttrList),
  InheritFrom(Expr, AttrList),
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Lambda {
  pub pos: Pos,
  pub name: Ident,
  pub arg: LambdaArg,
  pub body: ExprRef,
}

#[derive(Debug, Default, Clone, Serialize, Deserialize)]
pub struct Attrs {
  pub recursive: bool,
  pub attrs: HashMap<Ident, AttrDef>,
  pub dyn_attrs: Vec<DynAttrDef>,
}

impl Attrs {
  pub(crate) fn collect<I: IntoIterator<Item = Located<ParseBinding>>>(
    attrs_pos: Pos,
    bindings: I,
    allow_dyn: bool,
  ) -> Result<Self, Located<UserError>> {
    let mut this = Self::default();
    for item in bindings {
      match item.v {
        ParseBinding::Plain(path, rhs) => {
          this.add_attr(item.pos, &*path, rhs)?;
        }
        ParseBinding::Inherit(items) => {
          for Located { pos, v: item } in items {
            if let AttrName::Static(name) = item {
              if this.attrs.iter().any(|(x, _)| x == &name) {
                return Err(Located {
                  pos,
                  v: UserError::Other(format!("attribute `{}' already defined", name)),
                });
              } else {
                this.attrs.insert(
                  name.clone(),
                  AttrDef {
                    pos,
                    inherited: true,
                    rhs: Arc::new(Expr::Var { pos, name }),
                  },
                );
              }
            } else {
              return Err(Located {
                pos,
                v: UserError::Other("dynamic attributes not allowed here".into()),
              });
            }
          }
        }
        ParseBinding::InheritFrom(from, items) => {
          let f = Arc::new(from);
          for Located { pos, v: item } in items {
            if let AttrName::Static(name) = item {
              if this.attrs.contains_key(&name) {
                return Err(Located {
                  pos,
                  v: UserError::Other(format!("attribute `{}' already defined", name)),
                });
              } else {
                this.attrs.insert(
                  name.clone(),
                  AttrDef {
                    pos,
                    inherited: false,
                    rhs: Arc::new(Expr::Select {
                      pos,
                      lhs: f.clone(),
                      def: None,
                      path: vec![Located {
                        v: AttrName::Static(name),
                        pos,
                      }],
                    }),
                  },
                );
              }
            } else {
              return Err(Located {
                pos,
                v: UserError::Other("dynamic attributes not allowed here".into()),
              });
            }
          }
        }
      }
    }
    if !allow_dyn && !this.dyn_attrs.is_empty() {
      Err(Located {
        pos: attrs_pos,
        v: UserError::Other("dynamic attributes not allowed here".into()),
      })
    } else {
      Ok(this)
    }
  }

  fn add_attr(&mut self, pos: Pos, path: &[Located<AttrName>], rhs: Expr) -> ParseOk {
    match path {
      [] => panic!("invariant violation: empty attrpath produced by parser"),
      [Located {
        v: AttrName::Static(i),
        ..
      }] => {
        if let Some(x) = self.attrs.get_mut(i) {
          if let (Expr::Attrs(ae), Some(j)) =
            (rhs, Arc::get_mut(&mut x.rhs).unwrap().as_attrs_mut())
          {
            for (ad, av) in ae.attrs {
              if j.attrs.insert(ad, av).is_some() {
                panic!("duplicate attribute");
              }
            }
          } else {
            panic!("duplicate attribute")
          }
        } else {
          self.attrs.insert(
            i.clone(),
            AttrDef {
              pos,
              inherited: false,
              rhs: Arc::new(rhs),
            },
          );
        }
      }
      [Located {
        v: AttrName::Dynamic(e),
        ..
      }] => self.dyn_attrs.push(DynAttrDef {
        pos,
        name: e.clone(),
        value: Arc::new(rhs),
      }),
      [Located {
        v: AttrName::Static(i),
        ..
      }, rest @ ..] => {
        if let Some(j) = self.attrs.get_mut(i) {
          if !j.inherited {
            return Arc::get_mut(&mut j.rhs)
              .unwrap()
              .as_attrs_mut()
              .expect("duplicate attribute")
              .add_attr(pos, rest, rhs);
          } else {
            panic!("duplicate attribute")
          }
        } else {
          let mut new_attrs = Attrs::default();
          new_attrs.add_attr(pos, rest, rhs)?;
          self.attrs.insert(
            i.clone(),
            AttrDef {
              pos,
              inherited: false,
              rhs: Arc::new(Expr::Attrs(new_attrs)),
            },
          );
        }
      }
      [Located {
        v: AttrName::Dynamic(e),
        ..
      }, rest @ ..] => {
        let mut new_attrs = Attrs::default();
        new_attrs.add_attr(pos, rest, rhs)?;
        self.dyn_attrs.push(DynAttrDef {
          pos,
          name: e.clone(),
          value: Arc::new(Expr::Attrs(new_attrs)),
        });
      }
    }
    Ok(())
  }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AttrDef {
  pub pos: Pos,
  pub inherited: bool,
  pub rhs: ExprRef,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DynAttrDef {
  pub pos: Pos,
  pub name: ExprRef,
  pub value: ExprRef,
}

impl Display for Expr {
  fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
    match self {
      Expr::Pos { .. } => write!(f, "__curPos"),
      Expr::Var { name, .. } => name.fmt(f),
      Expr::Int { n, .. } => n.fmt(f),
      Expr::Float { f: g, .. } => g.fmt(f),
      Expr::String { s, .. } => write!(f, "{:?}", s),
      Expr::Path { path, .. } => path.display().fmt(f),
      Expr::Select { lhs, def, path, .. } => {
        write!(f, "({}).", lhs)?;
        show_attrpath(f, path)?;
        if let Some(x) = def {
          write!(f, " or ({})", x)?;
        }
        Ok(())
      }
      Expr::Lambda(Lambda { body, arg, .. }) => {
        write!(f, "(")?;
        match arg {
          LambdaArg::Plain(i) => write!(f, "{}", i)?,
          LambdaArg::Formals {
            name: Some(x),
            formals,
          } => write!(f, "{} @ {}", formals, x)?,
          LambdaArg::Formals {
            name: None,
            formals,
          } => write!(f, "{}", formals)?,
        }
        write!(f, ": {}", body)?;
        write!(f, ")")
      }
      Expr::List(l) => {
        write!(f, "[ ")?;
        for item in l {
          write!(f, "({}) ", item)?;
        }
        f.write_str("]")
      }
      Expr::Attrs(a) => {
        if a.recursive {
          write!(f, "rec ")?;
        }
        write!(f, "{{{}}}", a)
      }
      Expr::Assert { cond, body, .. } => write!(f, "assert {}; {}", cond, body),
      Expr::With { env, body, .. } => write!(f, "(with {}; {})", env, body),
      Expr::Let { attrs, body } => write!(f, "(let {} in {})", attrs, body),
      Expr::ConcatStrings { parts, .. } => {
        write!(f, "(")?;
        for (i, elem) in parts.iter().enumerate() {
          if i > 0 {
            write!(f, " + ")?;
          }
          elem.fmt(f)?;
        }
        write!(f, ")")
      }
      Expr::If {
        cond, rhs1, rhs2, ..
      } => write!(f, "(if {} then {} else {})", cond, rhs1, rhs2),
      Expr::Apply { lhs, rhs, .. } => write!(f, "({} {})", lhs, rhs),
      Expr::Op { bin, lhs, rhs, .. } => write!(f, "({} {} {})", lhs, bin, rhs),
      Expr::HasAttr { lhs, path } => {
        write!(f, "(({}) ? ", lhs)?;
        show_attrpath(f, path)?;
        f.write_str(")")
      }
      Expr::Not { e, .. } => write!(f, "(!{})", e),
    }
  }
}

impl Display for Attrs {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    for (i, (key, val)) in self.attrs.iter().enumerate() {
      if i > 0 {
        f.write_str(" ")?;
      }
      if val.inherited {
        write!(f, "inherit {};", key)?;
      } else {
        write!(f, "{} = {};", key, val.rhs)?;
      }
    }
    Ok(())
  }
}

impl Display for Formals {
  fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
    let mut first = true;
    write!(f, "{{ ")?;
    for item in &self.formals {
      if !first {
        write!(f, ", ")?;
      }
      first = false;
      write!(f, "{}", item.name)?;
      if let Some(e) = &item.def {
        write!(f, " ? {}", e)?;
      }
    }
    if self.ellipsis {
      if first {
        write!(f, "...")?
      } else {
        write!(f, ", ...")?
      }
    }
    if first {
      write!(f, "}}")
    } else {
      write!(f, " }}")
    }
  }
}
