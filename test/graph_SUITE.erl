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
    N2 = graph:add_node(G, <<"goodbye">>),
    ?assertEqual(2, graph:node_count(G)),
    ?assertEqual(nil, graph:find_edge(G, N1, N2)),
    ?assertMatch({badindex, _}, graph:get_edge(G, N2)),
    E1 = graph:add_edge(G, N1, N2, <<"the brief flash of light in between">>),
    ?assertEqual(<<"hello">>, graph:get_node(G, N1)),
    ?assertEqual(<<"goodbye">>, graph:get_node(G, N2)),
    ?assertEqual(<<"the brief flash of light in between">>, graph:get_edge(G, E1)),
    ?assertEqual(E1, graph:find_edge(G, N1, N2)),
    %% graph is unidirectional
    ?assertEqual(nil, graph:find_edge(G, N2, N1)),
    ?assertMatch({badindex, _}, graph:get_edge(G, N2)),
    ?assertEqual(<<"goodbye">>, graph:remove_node(G, N2)),
    ?assertMatch({badindex, _}, graph:get_edge(G, E1)),
    ?assertEqual(1, graph:node_count(G)),
    ?assertMatch({badindex, _}, graph:get_node(G, N2)),
    ?assertEqual(nil, graph:find_edge(G, N1, N2)),
    ok.
