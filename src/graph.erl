-module(graph).

-export([new/0, add_node/2, node_count/1]).

-on_load(load/0).

-opaque graph() :: reference().

-export_type([graph/0]).

-spec new() -> graph().
new() ->
    not_loaded(?LINE).

-spec node_count(graph()) -> number().
node_count(_Graph) ->
    not_loaded(?LINE).

-spec add_node(graph(), any()).
add_node(_Graph, _Term) ->
    not_loaded(?LINE).

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
