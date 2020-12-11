-module(graph).

-export([
    new/0,
    add_node/2,
    add_edge/4,
    node_count/1,
    get_node/2,
    get_edge/2,
    remove_node/2,
    remove_edge/2,
    find_edge/3,
    connected_components/1,
    tarjan_scc/1,
    neighbors/2,
    edges/2,
    fold_edges/4
]).

-on_load(load/0).

-opaque graph() :: reference().

-export_type([graph/0]).

-spec new() -> graph().
new() ->
    not_loaded(?LINE).

-spec node_count(graph()) -> pos_integer().
node_count(_Graph) ->
    not_loaded(?LINE).

-spec add_node(graph(), term()) -> pos_integer().
add_node(_Graph, _Term) ->
    not_loaded(?LINE).

add_edge(_Graph, _Start, _End, _Term) ->
    not_loaded(?LINE).

-spec get_node(graph(), pos_integer()) -> term() | {badindex, pos_integer()}.
get_node(_Graph, _Index) ->
    not_loaded(?LINE).

get_edge(_Graph, _Index) ->
    not_loaded(?LINE).

remove_node(_Graph, _Index) ->
    not_loaded(?LINE).

remove_edge(_Graph, _Index) ->
    not_loaded(?LINE).

find_edge(_Graph, _A, _B) ->
    not_loaded(?LINE).

connected_components(_Graph) ->
    not_loaded(?LINE).

tarjan_scc(_Graph) ->
    not_loaded(?LINE).

neighbors(_Graph, _Index) ->
    not_loaded(?LINE).

edges(_Graph, _Index) ->
    not_loaded(?LINE).

fold_edges(Fun, Acc, Graph, Start) when is_function(Fun, 2) ->
    SeenNodes = [Start],
    Nodes = neighbors(Graph, Start),
    Edges = [
        {Start, N, graph:find_edge(Graph, Start, N)}
        || N <- Nodes, nil /= graph:find_edge(Graph, Start, N)
    ],
    Acc2 = lists:foldl(Fun, Acc, Edges),
    fold_edges_int(Fun, Acc2, Graph, SeenNodes, Nodes).

fold_edges_int(_, Acc, _, _, []) ->
    Acc;
fold_edges_int(Fun, Acc, Graph, SeenNodes, [Node | Tail]) ->
    NewNodes = neighbors(Graph, Node) -- (SeenNodes ++ Tail),
    Edges = [
        {Node, N, graph:find_edge(Graph, Node, N)}
        || N <- neighbors(Graph, Node), nil /= graph:find_edge(Graph, Node, N)
    ],
    Acc2 = lists:foldl(Fun, Acc, Edges),
    fold_edges_int(Fun, Acc2, Graph, [Node | SeenNodes], Tail ++ NewNodes).

load() ->
    erlang:load_nif(filename:join(priv(), "libgraph"), none).

not_loaded(Line) ->
    erlang:nif_error({error, {not_loaded, [{module, ?MODULE}, {line, Line}]}}).

priv() ->
    case code:priv_dir(?MODULE) of
        {error, _} ->
            EbinDir = filename:dirname(code:which(?MODULE)),
            AppPath = filename:dirname(EbinDir),
            filename:join(AppPath, "priv");
        Path ->
            Path
    end.
