use std::{
  collections::{HashMap, HashSet},
  hash::Hash,
};

#[derive(Debug)]
pub struct DependencyQueue<N: Hash + Eq, E: Hash + Eq, V> {
  pub(super) dep_map: HashMap<N, (HashSet<(N, E)>, V)>,
  reverse_dep_map: HashMap<N, HashMap<E, HashSet<N>>>,
  priority: HashMap<N, usize>,
}

impl<N: Hash + Eq, E: Hash + Eq, V> Default for DependencyQueue<N, E, V> {
  fn default() -> DependencyQueue<N, E, V> {
    DependencyQueue::new()
  }
}

impl<N: Hash + Eq, E: Hash + Eq, V> DependencyQueue<N, E, V> {
  /// Creates a new dependency queue with 0 packages.
  pub fn new() -> DependencyQueue<N, E, V> {
    DependencyQueue {
      dep_map: HashMap::new(),
      reverse_dep_map: HashMap::new(),
      priority: HashMap::new(),
    }
  }
}

impl<N: Hash + Eq + Clone, E: Eq + Hash + Clone, V> DependencyQueue<N, E, V> {
  pub fn enqueue(&mut self, key: N, value: V, dependencies: impl IntoIterator<Item = (N, E)>) {
    assert!(!self.dep_map.contains_key(&key));

    let mut my_dependencies = HashSet::new();
    for (dep, edge) in dependencies {
      my_dependencies.insert((dep.clone(), edge.clone()));
      self
        .reverse_dep_map
        .entry(dep)
        .or_insert_with(HashMap::new)
        .entry(edge)
        .or_insert_with(HashSet::new)
        .insert(key.clone());
    }
    self.dep_map.insert(key, (my_dependencies, value));
  }

  pub fn queue_finished(&mut self) {
    let mut out = HashMap::new();
    for key in self.dep_map.keys() {
      depth(key, &self.reverse_dep_map, &mut out);
    }
    self.priority = out.into_iter().map(|(n, set)| (n, set.len())).collect();

    fn depth<'a, N: Hash + Eq + Clone, E: Hash + Eq + Clone>(
      key: &N,
      map: &HashMap<N, HashMap<E, HashSet<N>>>,
      results: &'a mut HashMap<N, HashSet<N>>,
    ) -> &'a HashSet<N> {
      if results.contains_key(key) {
        let depth = &results[key];
        assert!(!depth.is_empty(), "cycle in DependencyQueue");
        return depth;
      }
      results.insert(key.clone(), HashSet::new());

      let mut set = HashSet::new();
      set.insert(key.clone());

      for dep in map
        .get(key)
        .into_iter()
        .flat_map(|it| it.values())
        .flatten()
      {
        set.extend(depth(dep, map, results).iter().cloned())
      }

      let slot = results.get_mut(key).unwrap();
      *slot = set;
      &*slot
    }
  }

  pub fn dequeue(&mut self) -> Option<(N, V)> {
    let next = self
      .dep_map
      .iter()
      .filter(|(_, (deps, _))| deps.is_empty())
      .map(|(key, _)| key.clone())
      .max_by_key(|k| self.priority[k]);
    let key = match next {
      Some(key) => key,
      None => return None,
    };
    let (_, data) = self.dep_map.remove(&key).unwrap();
    Some((key, data))
  }

  pub fn is_empty(&self) -> bool {
    self.dep_map.is_empty()
  }

  pub fn len(&self) -> usize {
    self.dep_map.len()
  }

  pub fn finish(&mut self, node: &N, edge: &E) -> Vec<&N> {
    let reverse_deps = self.reverse_dep_map.get(node).and_then(|map| map.get(edge));
    let reverse_deps = match reverse_deps {
      Some(deps) => deps,
      None => return Vec::new(),
    };
    let key = (node.clone(), edge.clone());
    let mut result = Vec::new();
    for dep in reverse_deps.iter() {
      let edges = &mut self.dep_map.get_mut(dep).unwrap().0;
      assert!(edges.remove(&key));
      if edges.is_empty() {
        result.push(dep);
      }
    }
    result
  }
}
