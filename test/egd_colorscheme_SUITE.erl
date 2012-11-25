%%
%% Copyright (C) 2012 Hapida AB
%%
%% File:    egd_colorscheme_SUITE.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-11-25
%%
-module(egd_colorscheme_SUITE).

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
	hsl2rgb/1, 
	rgb2hsl/1
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
    [hsl2rgb, rgb2hsl].


check_rgb({R, G, B, _}) -> check_rgb({R, G, B});
check_rgb({R, G, B}) when
    R >= 0.0 andalso R =< 255.0 andalso
    G >= 0.0 andalso G =< 255.0 andalso
    B >= 0.0 andalso B =< 255.0 -> ok;
check_rgb(_) -> error.


hsl2rgb(_Config) ->
    Degs = [0,1,10,20,30,50, 100, 127,128,150,175,200,220,240,250,255, 300,310,320,327,340,351,359.9,360],
    Fs   = [ I/30  || I <- lists:seq(0, 30)],
    HSLs = [{H,S,L}||H <- Degs, S <- Fs, L <- Fs],

    ok = check_multiple_inputs(fun
	    (HSL) -> check_rgb(egd_colorscheme:hsl2rgb(HSL))
	end, HSLs),

    ok = check_multiple_inputs(fun
	    ({{R,G,B} = RGB, HSL}) ->
		{R1, G1, B1, _} = RGB1 = egd_colorscheme:hsl2rgb(HSL),
		io:format("hsl2rgb(~p) -> ~p (<- expected) ~p (<- got)~n", [
			HSL, RGB, RGB1]),
		true = check_value(R, R1),
		true = check_value(G, G1),
		true = check_value(B, B1),
		ok
    end, rgb_hsl_values()),
    ok.

check_hsl({H, S, L, _}) -> check_hsl({H, S, L});
check_hsl({H, S, L}) when
    H >= 0.0 andalso H < 360.0 andalso
    S >= 0.0 andalso S =< 1.0  andalso
    L >= 0.0 andalso L =< 1.0 -> ok;
check_hsl(_) -> error.

rgb2hsl(_Config) ->
    Octs = [0,1,10,20,30,50, 100, 127,128,150,175,200,220,240,250,255],
    RGBs = [{R,G,B} || R<-Octs, G <- Octs, B <- Octs],
    
    ok = check_multiple_inputs(fun
	    (RGB) -> check_hsl(egd_colorscheme:rgb2hsl(RGB))
	end, RGBs),

    ok = check_multiple_inputs(fun
	    ({RGB, {H, S, L} = HSL}) ->
		{H1, S1, L1, _} = HSL1 = egd_colorscheme:rgb2hsl(RGB),
		io:format("rgb2hsl(~p) -> ~p (<- expected) ~p (<- got)~n", [
			RGB, HSL, HSL1]),
		true = check_value(H, H1),
		true = check_value(S, S1),
		true = check_value(L, L1),
		ok
    end, rgb_hsl_values()),

    ok.

% aux

rgb_hsl_values() ->
    [
	{{255, 255, 255},{0,   0, 1}}, % white
	{{127, 127, 127},{0,   0, 0.5}}, % gray
	{{255, 0  , 0  },{0,   1, 0.5}}, % red
	{{127, 127, 255},{240, 1, 0.75}}
    ].

-define(float_error, 0.005).

check_value(V1, V1) -> true;
check_value(V1, V2) ->
    if
	abs(V1 - V2) < ?float_error -> true;
	true -> false
    end.

check_multiple_inputs(_, []) -> ok;
check_multiple_inputs(Fun, [I|Is]) ->
    case Fun(I) of
	ok -> check_multiple_inputs(Fun, Is);
	_  -> {failed, I}
    end.


