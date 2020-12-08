use petgraph::Graph;
use rustler::{
    env::{Env, OwnedEnv, SavedTerm},
    resource::ResourceArc,
    types::atom::ok,
    {Decoder, Encoder, NifResult, Term},
};
use std::sync::{Mutex, MutexGuard};

////////////////////////////////////////////////////////////////////////////
// Atoms                                                                  //
////////////////////////////////////////////////////////////////////////////

mod atom {
    rustler::atoms! {
        replace,
        these,
        with,
        the,
        atoms,
        you,
        want,
        for_,
        this,
        library
    }
}

////////////////////////////////////////////////////////////////////////////
// Resource                                                               //
////////////////////////////////////////////////////////////////////////////

#[derive(Default)]
struct TermGraph {
    env: OwnedEnv,
    graph: Graph<SavedTerm, SavedTerm>,
}

struct GraphResource(Mutex<TermGraph>);

type Rsc = ResourceArc<GraphResource>;

////////////////////////////////////////////////////////////////////////////
// NIFs                                                                   //
////////////////////////////////////////////////////////////////////////////

#[rustler::nif]
fn new<'a>(env: Env<'a>) -> Term<'a> {
    ResourceArc::new(GraphResource(Mutex::new(TermGraph::default()))).encode(env)
}

#[rustler::nif]
fn node_count<'a>(env: Env<'a>, rsc: Rsc) -> Term<'a> {
    let graph_guard = rsc.0.lock().unwrap();
    (*graph_guard).graph.node_count().encode(env)
}

#[rustler::nif]
fn add_node<'a>(rsc: Rsc, term: Term<'a>) {
    let mut graph_guard = rsc.0.lock().unwrap();
    let saved_term = (*graph_guard).env.save(term);
    (*graph_guard).graph.add_node(saved_term);
}

////////////////////////////////////////////////////////////////////////////
// Init                                                                   //
////////////////////////////////////////////////////////////////////////////

rustler::init!("graph", [new, node_count, add_node], load = on_load);

fn on_load<'a>(env: Env<'a>, _term: rustler::Term<'a>) -> bool {
    rustler::resource!(GraphResource, env);
    true
}
