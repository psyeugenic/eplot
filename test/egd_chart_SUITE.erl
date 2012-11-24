%%
%% Copyright (C) 2012 Hapida AB
%%
%% File:    egd_chart_SUITE.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-11-24
%%

-module(egd_chart_SUITE).

%% callbacks

-export([
	suite/0,
	init_per_suite/1,
	end_per_suite/1,
	init_per_testcase/2,
	end_per_testcase/2,
	all/0
    ]).

-export([
	graph_api/1,
	graph_api_opts/1
    ]).


-include_lib("common_test/include/ct.hrl").

suite() ->
    [{timetrap,{seconds,180}}].

init_per_suite(Config) ->
    Config.

end_per_suite(_Config) ->
    ok.

init_per_testcase(_TestCase, Config) ->
    Config.

end_per_testcase(_TestCase, _Config) ->
    ok.

groups() ->
    [].

all() -> 
    [graph_api, graph_api_opts].


make_simple_set(Min, Max, Step) ->
    [{X,30*math:sin(X)} || X <- lists:seq(Min,Max,Step)].

check_chart_result(B) when is_binary(B) -> ok.

graph_api(_Config) -> 
    D1 = [{"graph 1", make_simple_set(0, 100, 1)}],
    ok = check_chart_result(egd_chart:graph(D1)),

    D2 = [{"graph 2", make_simple_set(-100, 100, 1)}|D1],
    ok = check_chart_result(egd_chart:graph(D2)),

    D3 = [{"graph 3", make_simple_set(-1000, 1000, 3)}|D2],
    ok = check_chart_result(egd_chart:graph(D3)),

    D4 = [{graph_4, make_simple_set(-1000, 1000, 3)}|D3],
    ok = check_chart_result(egd_chart:graph(D4)),

    D5 = [{graph_5, make_simple_set(-1000, 10000, 1)}|D4],
    ok = check_chart_result(egd_chart:graph(D5)),

    D6 = [{graph_6, make_simple_set(-1000, 10000, 1)}],
    ok = check_chart_result(egd_chart:graph(D6)),

    D7 = [{"graph 7", make_simple_set(-300, -10, 1)}|D6],
    ok = check_chart_result(egd_chart:graph(D7)),


    ok.

check_graph_api_opts(_, []) -> ok;
check_graph_api_opts(Data, [Opts|Os]) ->
    io:format("~p - Data~n", [now()]),
    ok = check_chart_result(egd_chart:graph(Data, Opts)),
    check_graph_api_opts(Data, Os).


graph_api_opts(_Config) -> 
    Options   = [[
	{width, Width}, {height, Height}, {margin, Margin}, 
	{ticksize, Ticksize}, {x_range, Xrange}, {y_range, Yrange},
	{y_label, Ylabel}, {x_label, Xlabel}, {bg_rgba, Rgba}] ||

	Width    <- [800, 1300],
	Height   <- [800, 1024],
	Margin   <- [30, 60],
	Ticksize <- [{12,12}, {180,180}],
	Xrange   <- [{0, 100}, {-1000, 1000}],
	Yrange   <- [{0, 100}, {-1000, 1000}],
	Ylabel   <- ["Y-label"],
	Xlabel   <- ["X-label"],
	Rgba     <- [{255,255,255}, {20,25,100,120}]
    ],
    D1 = [{"graph 1", make_simple_set(0, 1200, 60)}],
    ok = check_graph_api_opts(D1, Options),

    D2 = [{graph_2, make_simple_set(-30, -10, 3)}|D1],
    ok = check_graph_api_opts(D2, Options),

    ok.
