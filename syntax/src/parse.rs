use super::Expr;
use crate::util::*;
pub use generated::*;
use std::path::PathBuf;

mod generated {
  #![allow(clippy::all)]
  include!(concat!(env!("OUT_DIR"), "/parse.rs"));
}

fn homedir() -> PathBuf {
  std::env::var("HOME")
    .expect("variable $HOME not set")
    .into()
}

fn unindent(pos: Pos, es: Vec<Expr>) -> Expr {
  if es.is_empty() {
    return Expr::String { s: String::new() };
  }

  let mut at_start_of_line = true;
  let mut min_indent = 1_000_000usize;
  let mut cur_indent = 0;

  for expr in &es {
    if let Some(dy) = expr.as_string() {
      for ch in dy.chars() {
        if at_start_of_line {
          if ch == ' ' {
            cur_indent += 1;
          } else if ch == '\n' {
            cur_indent = 0;
          } else {
            at_start_of_line = false;
            if cur_indent < min_indent {
              min_indent = cur_indent;
            }
          }
        } else if ch == '\n' {
          at_start_of_line = true;
          cur_indent = 0;
        }
      }
    } else {
      if at_start_of_line {
        at_start_of_line = false;
        if cur_indent < min_indent {
          min_indent = cur_indent;
        }
      }
      continue;
    }
  }

  let mut es2 = vec![];
  at_start_of_line = true;
  let mut cur_dropped = 0;

  for (n, expr) in es.into_iter().rev().enumerate().rev() {
    match expr.into_string() {
      Ok(s) => {
        let mut s2 = String::new();
        for ch in s.chars() {
          if at_start_of_line {
            if ch == ' ' {
              if cur_dropped >= min_indent {
                s2.push(ch);
              }
              cur_dropped += 1;
            } else if ch == '\n' {
              cur_dropped = 0;
              s2.push(ch);
            } else {
              at_start_of_line = false;
              cur_dropped = 0;
              s2.push(ch);
            }
          } else {
            s2.push(ch);
            if ch == '\n' {
              at_start_of_line = true;
            }
          }
        }

        if n == 0 {
          if let Some(p) = s2.rfind('\n') {
            if s2[p..].chars().all(|x| x == ' ') {
              s2.truncate(p + 1);
            }
          }
        }

        es2.push(Expr::String { s: s2 });
      }
      Err(other) => {
        at_start_of_line = false;
        cur_dropped = 0;
        es2.push(other);
      }
    }
  }

  if es2.len() == 1 && es2[0].as_string().is_some() {
    es2.pop().unwrap()
  } else {
    Expr::ConcatStrings {
      pos,
      force_string: true,
      parts: es2,
    }
  }
}
