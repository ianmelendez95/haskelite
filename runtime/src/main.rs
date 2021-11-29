mod types;
mod builtins;
mod gmachine;

use types::*;
use std::collections::HashMap;
use crate::builtins as bi;

fn main() {
  let mut state: GState = gmachine::init_state();

  g(&mut state);
  bi::print(&state);
}

// g f = NEG (f 5)
fn g(state: &mut GState) {
  bi::push_int(state, 5);
  bi::push_stack(state, 1);
  bi::mk_ap(state);
  bi::push_proc(state, neg);
  bi::mk_ap(state);
  bi::update(state, 2);
  bi::pop_n(state, 1);
}

fn neg(state: &mut GState) {
  bi::eval(state);
  bi::neg(state);
  bi::update(state, 1);
  bi::g_return(state);
}

fn plus(state: &mut GState) {
  bi::push_stack(state, 1);
  bi::eval(state);
  bi::push_stack(state, 1);
  bi::add(state);
  bi::update(state, 3);
  bi::pop_n(state, 2);
  bi::unwind(state);
}
