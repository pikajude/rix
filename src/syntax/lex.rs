use super::UserError;
use crate::util::*;
use codespan::{FileId, Span};
use regex::Regex;
use std::num::{ParseFloatError, ParseIntError};

#[derive(Clone, Debug, Display)]
pub enum Token<'a> {
  If,
  Then,
  Else,
  Assert,
  With,
  Let,
  In,
  Rec,
  Inherit,
  OrKw,
  Ellipsis,

  Eq,
  Neq,
  Leq,
  Geq,
  And,
  Or,
  Impl,
  Update,
  Concat,

  Id(Ident),
  Int(i64),
  Float(f64),

  DollarCurly,
  IndStringOpen,
  IndStringClose,

  Str(String),
  IndStr(String),
  Path(&'a str),
  HomePath(&'a str),
  NixPath(&'a str),
  Uri(&'a str),
  Any(char),
}

lazy_static! {
  pub static ref ID: Regex = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_'-]*").unwrap();
  static ref INT: Regex = Regex::new(r"^[0-9]+").unwrap();
  static ref FLOAT: Regex =
    Regex::new(r"^(([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][+-]?[0-9]+)?").unwrap();
  static ref PATH: Regex = Regex::new(r"^[a-zA-Z0-9\._\-\+]*(/[a-zA-Z0-9\._\-\+]+)+/?").unwrap();
  static ref HPATH: Regex = Regex::new(r"^\~(/[a-zA-Z0-9\._\-\+]+)+/?").unwrap();
  static ref SPATH: Regex = Regex::new(r"^<[a-zA-Z0-9\._\-\+]+(/[a-zA-Z0-9\._\-\+]+)*>").unwrap();
  static ref URI: Regex =
    Regex::new(r"^[a-zA-Z][a-zA-Z0-9\+\-\.]*:[a-zA-Z0-9%/\?:@\&=\+\$,\-_\.!\~\*']+").unwrap();
  static ref OPER: Regex = Regex::new(r"^(\.\.\.|==|!=|<=|>=|&&|\|\||->|//|\+\+)").unwrap();
  static ref STRING_INNER_0: Regex =
    Regex::new(r#"^([^\$"\\]|\$[^\{"\\]|\\(?s).|\$\\(?s).)*\$/""#).unwrap();
  static ref STRING_INNER_1: Regex =
    Regex::new(r#"^([^\$"\\]|\$[^\{"\\]|\\(?s).|\$\\(?s).)+"#).unwrap();
  static ref STRING_END_INVALID: Regex = Regex::new(r"^(\$|\\|\$\\)").unwrap();
  static ref IND_STRING_START: Regex = Regex::new(r"^''( *\n)?").unwrap();
  static ref IND_STRING_INNER: Regex = Regex::new(r"^([^\$']|\$[^\{']|'[^'\$])+").unwrap();
  static ref IND_STRING_ANY: Regex = Regex::new(r"^''\\(?s).").unwrap();
  static ref WHITESPACE: Regex = Regex::new(r"^[ \t\r\n]+").unwrap();
  static ref COMMENT_SINGLE: Regex = Regex::new(r"^#[^\r\n]*").unwrap();
  static ref COMMENT_MULTI: Regex = Regex::new(r"^/\*([^*]|\*+[^*/])*\*+/").unwrap();
}

#[derive(Clone, Copy, Debug)]
enum Mode {
  Default,
  String,
  IndString,
}

#[derive(Debug, Display)]
pub enum LexError {
  InvalidFloat(ParseFloatError),
  InvalidInt(ParseIntError),
  UnrecognizedInput,
}

#[derive(Clone)]
pub struct Lexer<'a> {
  _input: &'a str,
  at: usize,
  mode: Vec<Mode>,
  id: FileId,
}

impl<'a> Lexer<'a> {
  pub fn new(input: &'a str, id: FileId) -> Self {
    Self {
      _input: input,
      at: 0,
      mode: vec![],
      id,
    }
  }

  fn input(&self) -> &'a str {
    &self._input[self.at..]
  }

  fn mode(&self) -> Mode {
    self.mode.last().copied().unwrap_or(Mode::Default)
  }
}

impl<'a> Iterator for Lexer<'a> {
  type Item = Result<(usize, Token<'a>, usize), Located<UserError>>;

  fn next(&mut self) -> Option<Self::Item> {
    if self.input().is_empty() {
      return None;
    }

    let start = self.at;
    let input = self.input();

    macro_rules! token_ {
      ($regex:ident, $cb:expr) => {
        if let Some(m) = $regex.find(input) {
          debug_assert!(m.start() == 0);
          self.at += m.end();
          return Some($cb(m.as_str()).map(|n| (start, n, self.at)));
        }
      };
    }

    macro_rules! token {
      ($regex:ident, $cb:expr) => {
        if let Some(m) = $regex.find(input) {
          debug_assert!(m.start() == 0);
          self.at += m.end();
          return Some(Ok((start, $cb(m.as_str()), self.at)));
        }
      };
    }

    macro_rules! skip {
      ($regex:ident) => {
        if let Some(m) = $regex.find(input) {
          debug_assert!(m.start() == 0);
          self.at += m.end();
          return self.next();
        }
      };
    }

    macro_rules! lit {
      ($lit:literal, $cb:expr) => {
        if input.starts_with($lit) {
          self.at += $lit.len();
          return Some(Ok((start, $cb(), self.at)));
        }
      };
    }

    match self.mode() {
      Mode::Default => {
        token!(PATH, Token::Path);
        token!(HPATH, Token::HomePath);
        token!(SPATH, Token::NixPath);
        token!(URI, Token::Uri);

        token!(ID, |s| match s {
          "if" => Token::If,
          "then" => Token::Then,
          "else" => Token::Else,
          "assert" => Token::Assert,
          "with" => Token::With,
          "let" => Token::Let,
          "in" => Token::In,
          "rec" => Token::Rec,
          "inherit" => Token::Inherit,
          "or" => Token::OrKw,
          x => Token::Id(Ident::from(x)),
        });

        token!(OPER, |s| match s {
          "..." => Token::Ellipsis,
          "==" => Token::Eq,
          "!=" => Token::Neq,
          "<=" => Token::Leq,
          ">=" => Token::Geq,
          "&&" => Token::And,
          "||" => Token::Or,
          "->" => Token::Impl,
          "//" => Token::Update,
          "++" => Token::Concat,
          _ => unreachable!(),
        });

        token_!(INT, |i: &str| Ok(Token::Int(match i.parse::<i64>() {
          Ok(i) => i,
          Err(i) =>
            return Err(Located {
              pos: Pos(self.id, Span::new(start as u32, self.at as u32)),
              v: UserError::Lexer(LexError::InvalidInt(i))
            }),
        })));
        token_!(FLOAT, |i: &str| Ok(Token::Float(match i.parse::<f64>() {
          Ok(i) => i,
          Err(i) =>
            return Err(Located {
              pos: Pos(self.id, Span::new(start as u32, self.at as u32)),
              v: UserError::Lexer(LexError::InvalidFloat(i))
            }),
        })));

        lit!("${", || {
          self.mode.push(Mode::Default);
          Token::DollarCurly
        });
        lit!("}", || {
          self.mode.pop();
          Token::Any('}')
        });
        lit!("{", || {
          self.mode.push(Mode::Default);
          Token::Any('{')
        });
        lit!("\"", || {
          self.mode.push(Mode::String);
          Token::Any('"')
        });

        token!(IND_STRING_START, |_: &str| {
          self.mode.push(Mode::IndString);
          Token::IndStringOpen
        });

        skip!(WHITESPACE);
        skip!(COMMENT_SINGLE);
        skip!(COMMENT_MULTI);

        let ch = input.chars().next().unwrap();
        self.at += ch.len_utf8();
        return Some(Ok((start, Token::Any(ch), self.at)));
      }
      Mode::String => {
        token!(STRING_INNER_0, |x| Token::Str(unescape_str(x)));
        token!(STRING_INNER_1, |x| Token::Str(unescape_str(x)));

        lit!("${", || {
          self.mode.push(Mode::Default);
          Token::DollarCurly
        });
        lit!("\"", || {
          self.mode.pop();
          Token::Any('"')
        });

        token!(STRING_END_INVALID, |x: &str| Token::Str(x.to_string()));
      }
      Mode::IndString => {
        token!(IND_STRING_INNER, |x: &str| Token::IndStr(x.to_string()));
        lit!("''$", || Token::IndStr("$".into()));
        lit!("${", || {
          self.mode.push(Mode::Default);
          Token::DollarCurly
        });
        lit!("$", || Token::IndStr("$".into()));
        lit!("'''", || Token::IndStr("''".into()));
        token!(IND_STRING_ANY, |x| Token::IndStr(unescape_str(x)));
        lit!("''", || {
          self.mode.pop();
          Token::IndStringClose
        });
        lit!("'", || Token::IndStr("'".into()));
      }
    }

    Some(Err(Located {
      pos: Pos(self.id, Span::new(start as u32, self.at as u32)),
      v: UserError::Lexer(LexError::UnrecognizedInput),
    }))
  }
}

fn unescape_str(string: &str) -> String {
  let mut s = String::with_capacity(string.len());
  let mut char_iter = string.chars().peekable();
  while let Some(mut c) = char_iter.next() {
    if c == '\\' {
      c = char_iter.next().expect("escape at end of string");
      match c {
        'n' => s.push('\n'),
        'r' => s.push('\r'),
        't' => s.push('\t'),
        x => s.push(x),
      }
    } else if c == '\r' {
      s.push('\n');
      if char_iter.peek() == Some(&'\n') {
        char_iter.next();
      }
    } else {
      s.push(c)
    }
  }
  s
}

/*
pub fn strip_indentation(mut parts: Vec<StrPart>) -> Vec<StrPart> {
  if parts.is_empty() {
    return parts;
  }

  let mut at_start_of_line = true;
  let mut min_indent: usize = 1_000_000;
  let mut cur_indent: usize = 0;

  for part in &parts {
    match part {
      StrPart::Quote { .. } => {
        if at_start_of_line {
          at_start_of_line = false;
          if cur_indent < min_indent {
            min_indent = cur_indent;
          }
        }
      }
      StrPart::Plain(s) => {
        for ch in s.chars() {
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
      }
    }
  }

  at_start_of_line = true;
  let mut cur_dropped = 0usize;

  let is_single_parts = parts.len() == 1;

  for part in &mut parts {
    match part {
      StrPart::Quote { .. } => {
        at_start_of_line = false;
        cur_dropped = 0;
      }
      StrPart::Plain(s) => {
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

        if is_single_parts {
          if let Some(last_nl) = s2.rfind('\n') {
            if s2[last_nl..].bytes().all(|x| x == b' ') {
              s2 = s2[..last_nl].to_string();
            }
          }
        }

        *s = s2;
      }
    }
  }

  parts
}
*/
