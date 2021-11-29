mod types;
mod builtins;
mod gmachine;

use types::*;
use std::collections::HashMap;
use crate::builtins::*;

fn main() {
  let mut state: GState = gmachine::init_state();

  g(&mut state);
}

// g f = NEG (f 5)
fn g(state: &mut GState) {
  push_int(state, 5);
  push_stack(state, 1);
  mk_ap(state);
  push_proc(state, neg);
  mk_ap(state);
  update(state, 2);
  pop_n(state, 1);
}
