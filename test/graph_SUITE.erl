-module(graph_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
    init_per_suite/1,
    end_per_suite/1,
    all/0
]).

-export([basic_test/1]).

all() ->
    [
        basic_test
    ].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

basic_test(_Config) ->
    G = graph:new(),
    ?assertEqual(0, graph:node_count(G)),
    N1 = graph:add_node(G, <<"hello">>),
    ?assertEqual(1, graph:node_count(G)),
    ?assertEqual([N1], graph:connected_components(G)),
    N2 = graph:add_node(G, <<"goodbye">>),
    ?assertEqual([N1, N2], graph:connected_components(G)),
    ?assertEqual(2, graph:node_count(G)),
    ?assertEqual(nil, graph:find_edge(G, N1, N2)),
    ?assertMatch({badindex, _}, graph:get_edge(G, N2)),
    ?assertEqual([[N1], [N2]], graph:tarjan_scc(G)),
    E1 = graph:add_edge(G, N1, N2, <<"the brief flash of light in between">>),
    ?assertEqual([E1], graph:edges(G, N1)),
    ?assertEqual([N2], graph:neighbors(G, N1)),
    ?assertEqual([[N2], [N1]], graph:tarjan_scc(G)),
    ?assertEqual(nil, graph:find_edge(G, N2, N1)),
    %% graph is unidirectional
    ?assertMatch({badindex, _}, graph:get_edge(G, N2)),
    E2 = graph:add_edge(G, N2, N1, <<"time's arrow flies backwards">>),
    ?assertEqual([E2, E1], graph:fold_edges(fun(E, Acc) -> [E | Acc] end, [], G, N1)),
    ?assertEqual(
        [<<"the brief flash of light in between">>, <<"time's arrow flies backwards">>],
        graph:fold_edges(fun(E, Acc) -> [graph:get_edge(G, E) | Acc] end, [], G, N2)
    ),
    ?assertEqual([E1], graph:edges(G, N1)),
    ?assertEqual([[N2, N1]], graph:tarjan_scc(G)),
    ?assertEqual([N1], graph:connected_components(G)),
    ?assertEqual(<<"hello">>, graph:get_node(G, N1)),
    ?assertEqual(<<"goodbye">>, graph:get_node(G, N2)),
    ?assertEqual(<<"the brief flash of light in between">>, graph:get_edge(G, E1)),
    ?assertEqual(<<"time's arrow flies backwards">>, graph:get_edge(G, E2)),
    ?assertEqual(E1, graph:find_edge(G, N1, N2)),
    ?assertEqual(<<"goodbye">>, graph:remove_node(G, N2)),
    ?assertMatch({badindex, _}, graph:get_edge(G, E1)),
    ?assertEqual(1, graph:node_count(G)),
    ?assertMatch({badindex, _}, graph:get_node(G, N2)),
    ?assertEqual(nil, graph:find_edge(G, N1, N2)),
    ?assertEqual([[N1]], graph:tarjan_scc(G)),
    ?assertEqual([], graph:neighbors(G, N1)),
    ok.
