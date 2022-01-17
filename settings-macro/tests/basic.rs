use std::collections::HashSet;

use rix_settings_macro::Settings;

#[derive(Settings)]
pub struct Settings {
  #[setting(get = "deref")]
  foo: String,
  /// very nice thing
  #[setting(get = "ref", default_fn = "HashSet::new")]
  thing: HashSet<String>,
  num_cpus: usize,
}

#[test]
fn test_foo() {
  let x = Settings {
    foo: String::default(),
    thing: Default::default(),
    num_cpus: 10,
  };
  println!("{}", x.foo());
}
