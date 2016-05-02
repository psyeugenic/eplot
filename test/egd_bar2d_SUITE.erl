%%
%% Copyright (C) 2016 Björn-Egil Dahlberg
%%
%% File:    egd_bar2d_SUITE.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2016-05-02
%%

-module(egd_bar2d_SUITE).
-include_lib("common_test/include/ct.hrl").

%% callbacks

-export([all/0, suite/0]).

-export([bar2d_api/1]).

suite() ->
    [{timetrap,{seconds,180}}].

all() -> 
    [bar2d_api].

bar2d_api(_Config) -> 
    D1 = [{"graph 1", make_simple_set(0, 100, 12)}],
    ok = check_chart_result(egd_chart:bar2d(D1)),

    D2 = [{"graph 2", make_simple_set(-100, 100, 13)}|D1],
    ok = check_chart_result(egd_chart:bar2d(D2)),

    D3 = [{"graph 3", make_simple_set(-1000, 1000, 30)}|D2],
    ok = check_chart_result(egd_chart:bar2d(D3)),

    D4 = [{graph_4, make_simple_set(-1000, 1000, 3)}|D3],
    ok = check_chart_result(egd_chart:bar2d(D4)),

    D5 = [{graph_5, make_simple_set(-1000, 10000, 111)}|D4],
    ok = check_chart_result(egd_chart:bar2d(D5)),

    D6 = [{graph_6, make_simple_set(-1000, 10000, 57)}],
    ok = check_chart_result(egd_chart:bar2d(D6)),

    D7 = [{"graph 7", make_simple_set(-300, -10, 1)}|D6],
    ok = check_chart_result(egd_chart:bar2d(D7)),
    ok.

check_chart_result(B) when is_binary(B) -> ok.

make_simple_set(Min, Max, Step) ->
    [{"bar " ++ integer_to_list(X),30*math:sin(X) + 30} || X <- lists:seq(Min,Max,Step)].
