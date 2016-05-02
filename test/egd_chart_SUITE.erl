%%
%% Copyright (C) 2016 Björn-Egil Dahlberg
%%
%% File:    egd_chart_SUITE.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-11-24
%%

-module(egd_chart_SUITE).
-include_lib("common_test/include/ct.hrl").

%% callbacks

-export([all/0, suite/0]).

-export([graph_api/1,
         graph_api_opts/1]).

suite() ->
    [{timetrap,{seconds,180}}].

all() -> 
    [graph_api, graph_api_opts].

graph_api(_Config) -> 
    D1 = [{"graph 1", make_simple_set(0, 100, 12)}],
    ok = check_chart_result(egd_chart:graph(D1)),

    D2 = [{"graph 2", make_simple_set(-100, 100, 13)}|D1],
    ok = check_chart_result(egd_chart:graph(D2)),

    D3 = [{"graph 3", make_simple_set(-1000, 1000, 30)}|D2],
    ok = check_chart_result(egd_chart:graph(D3)),

    D4 = [{graph_4, make_simple_set(-1000, 1000, 3)}|D3],
    ok = check_chart_result(egd_chart:graph(D4)),

    D5 = [{graph_5, make_simple_set(-1000, 10000, 111)}|D4],
    ok = check_chart_result(egd_chart:graph(D5)),

    D6 = [{graph_6, make_simple_set(-1000, 10000, 57)}],
    ok = check_chart_result(egd_chart:graph(D6)),

    D7 = [{"graph 7", make_simple_set(-300, -10, 1)}|D6],
    ok = check_chart_result(egd_chart:graph(D7)),
    ok.

graph_api_opts(_Config) -> 
    Options = [
        [{width, Width}, {height, Height}, {margin, Margin}, 
         {ticksize, Ticksize}, {x_range, Xrange}, {y_range, Yrange},
         {y_label, Ylabel}, {x_label, Xlabel}, {bg_rgba, Rgba}] ||

        Width    <- [400, 1300],
        Height   <- [400, 1024],
	Margin   <- [60],
	Ticksize <- [{12,12}, {180,180}],
	Xrange   <- [{0, 100}, {-1000, 1000}],
	Yrange   <- [{0, 100}, {-1000, 1000}],
	Ylabel   <- ["Y-label"],
	Xlabel   <- ["X-label"],
	Rgba     <- [{255,255,255}]
    ],
    D1 = [{"graph 1", make_simple_set(0, 1200, 360)}],
    ok = check_graph_api_opts(D1, Options),
    ok.

check_graph_api_opts(_, []) -> ok;
check_graph_api_opts(Data, [Opts|Os]) ->
    T0 = os:timestamp(),
    ok = check_chart_result(egd_chart:graph(Data, Opts)),
    T1 = os:timestamp(),
    io:format("graph ~.2f s~n", [timer:now_diff(T1,T0) / 1000000]),
    check_graph_api_opts(Data, Os).

check_chart_result(B) when is_binary(B) -> ok.

make_simple_set(Min, Max, Step) ->
    [{X,30*math:sin(X)} || X <- lists:seq(Min,Max,Step)].
