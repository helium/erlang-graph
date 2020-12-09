use petgraph::{graph::EdgeIndex, graph::NodeIndex, Graph};
use rustler::{
    env::{Env, OwnedEnv, SavedTerm},
    resource::ResourceArc,
    Encoder, Term,
};
use std::sync::Mutex;

////////////////////////////////////////////////////////////////////////////
// Atoms                                                                  //
////////////////////////////////////////////////////////////////////////////

mod atom {
    rustler::atoms! {
        badindex
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
fn new() -> Rsc {
    ResourceArc::new(GraphResource(Mutex::new(TermGraph::default())))
}

#[rustler::nif]
fn node_count(rsc: Rsc) -> usize {
    let graph_guard = rsc.0.lock().unwrap();
    (*graph_guard).graph.node_count()
}

#[rustler::nif]
fn add_node(rsc: Rsc, term: Term<'_>) -> usize {
    let mut graph_guard = rsc.0.lock().unwrap();
    let saved_term = (*graph_guard).env.save(term);
    (*graph_guard).graph.add_node(saved_term).index()
}

#[rustler::nif]
fn add_edge(rsc: Rsc, a: usize, b: usize, term: Term<'_>) -> usize {
    let mut graph_guard = rsc.0.lock().unwrap();
    let saved_term = (*graph_guard).env.save(term);
    (*graph_guard)
        .graph
        .add_edge(NodeIndex::new(a), NodeIndex::new(b), saved_term)
        .index()
}

#[rustler::nif]
fn get_node(env: Env<'_>, rsc: Rsc, idx: usize) -> Term<'_> {
    let graph_guard = rsc.0.lock().unwrap();
    let tg = &*graph_guard;
    tg.graph
        .node_weight(NodeIndex::new(idx))
        .map(|term| tg.env.run(|e| term.load(e).in_env(env)))
        .unwrap_or_else(|| (atom::badindex(), idx).encode(env))
}

#[rustler::nif]
fn get_edge(env: Env<'_>, rsc: Rsc, idx: usize) -> Term<'_> {
    let graph_guard = rsc.0.lock().unwrap();
    let tg = &*graph_guard;
    tg.graph
        .edge_weight(EdgeIndex::new(idx))
        .map(|term| tg.env.run(|e| term.load(e).in_env(env)))
        .unwrap_or_else(|| (atom::badindex(), idx).encode(env))
}

#[rustler::nif]
fn remove_node(env: Env<'_>, rsc: Rsc, idx: usize) -> Option<Term<'_>> {
    let mut graph_guard = rsc.0.lock().unwrap();
    (*graph_guard)
        .graph
        .remove_node(NodeIndex::new(idx))
        .map(|term| (*graph_guard).env.run(|e| term.load(e).in_env(env)))
}

#[rustler::nif]
fn remove_edge(env: Env<'_>, rsc: Rsc, idx: usize) -> Option<Term<'_>> {
    let mut graph_guard = rsc.0.lock().unwrap();
    (*graph_guard)
        .graph
        .remove_edge(EdgeIndex::new(idx))
        .map(|term| (*graph_guard).env.run(|e| term.load(e).in_env(env)))
}

////////////////////////////////////////////////////////////////////////////
// Init                                                                   //
////////////////////////////////////////////////////////////////////////////

rustler::init!(
    "graph",
    [
        new,
        node_count,
        add_node,
        add_edge,
        get_node,
        get_edge,
        remove_node,
        remove_edge
    ],
    load = on_load
);

fn on_load<'a>(env: Env<'a>, _term: rustler::Term<'a>) -> bool {
    rustler::resource!(GraphResource, env);
    true
}
