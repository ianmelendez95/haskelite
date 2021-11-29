use std::collections::HashMap;
use super::types::*;

pub fn init_state() -> GState {
  return GState {
    stack: Vec::new(),
    graph: HashMap::new(),
    prog: Vec::new(),
    dump: Vec::new()
  };
}