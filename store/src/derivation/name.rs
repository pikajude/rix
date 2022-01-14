pub struct Name {
  pub name: String,
  pub version: String,
}

impl Name {
  pub fn new<S: Into<String>>(s: S) -> Self {
    let mut this = Self {
      name: s.into(),
      version: String::new(),
    };

    for (ix, ch) in this.name.char_indices() {
      let next_ix = ix + ch.len_utf8();
      if ch == '-'
        && this.name.len() >= next_ix
        && this.name[next_ix..].starts_with(|x: char| !x.is_ascii_alphabetic())
      {
        this.version = this.name.split_off(next_ix);
        this.name.truncate(this.name.len() - 1);
        break;
      }
    }

    this
  }
}

#[test]
fn testname() {
  let n = Name::new("apache-httpd-2.0.48");
  assert_eq!(n.name, "apache-httpd");
  assert_eq!(n.version, "2.0.48");

  let n = Name::new("apache-httpd-");
  assert_eq!(n.name, "apache-httpd-");
  assert_eq!(n.version, "");
}
