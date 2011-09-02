%% Copyright (C) 2011 Björn-Egil Dahlberg
%%
%% File:    egd_colorscheme.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2011-09-02

-module(egd_colorscheme).

-export([
	test/1,
	hsl2rgb/1, 
	rgb2hsl/1,
	select/2
    ]).

test(File) ->
    Im = egd:create(800,600),
    lists:foreach(fun
	    (I) ->
		egd:filledRectangle(Im, {(I - 1)*20, 0}, {(I - 1)*20 + 19, 20}, egd_colorscheme:select(default, I))
	end, lists:seq(1,40)),
    B = egd:render(Im),
    egd:save(B, File),
    egd:destroy(Im),
    ok.

% L - low dark, high bright
% S

select(core1, I) -> select(I, 17, 0.3, 0.3, 200);
select(_Default, I) -> select(I + 6, 31, 0.8, 0.4, 210).
select(I, Hm, S, L, A) -> egd:color(hsl2rgb({I*Hm rem 360, S, L, A})).

%% color conversions
%% H, hue has the range of [0, 360]
%% S, saturation has the range of [0,1]
%% L, lightness has the range of [0,1]

-define(float_error, 0.0000000000000001).

hsl2rgb({H,S,L}) -> hsl2rgb({H,S,L,255});
hsl2rgb({H,S,L,A}) ->
    Q  = if
	L < 0.5 -> L * (1 + S);
	true    -> L + S - (L * S)
    end,
    P  = 2 * L - Q,
    Hk = H/360,
    Rt = Hk + 1/3,
    Gt = Hk,
    Bt = Hk - 1/3,

    Cts = lists:map(fun
	(Tc) when Tc < 0.0 -> Tc + 1.0;
	(Tc) when Tc > 1.0 -> Tc - 1.0;
	(Tc) ->  Tc
    end, [Rt, Gt, Bt]),
    [R,G,B] = lists:map(fun
	(Tc) when Tc < 1/6 -> P + ((Q - P) * 6 * Tc);
	(Tc) when Tc < 1/2, Tc >= 1/6 -> Q;
	(Tc) when Tc < 2/3, Tc >= 1/2 -> P + ((Q - P) * 6 * (2/3 - Tc));
	(_ ) -> P
    end, Cts),
    {trunc(R*255),trunc(G*255),trunc(B*255),A}.

rgb2hsl({R,G,B}) -> rgb2hsl({R,G,B,255});
rgb2hsl({R,G,B,A}) ->
    Rf  = R/255,
    Gf  = G/255,
    Bf  = B/255,
    Max = lists:max([Rf,Gf,Bf]),
    Min = lists:min([Rf,Gf,Bf]),
    H   = if
	    abs(Max - Min) < ?float_error ->
		0;
	    abs(Max - Rf)  < ?float_error ->
		D  = 60 * (Gf - Bf)/(Max - Min),
		Dt = trunc(D),
	        Dt rem 360;
	    abs(Max - Gf) < ?float_error ->
		60 * (Bf - Rf)/(Max - Min) + 120;
	    abs(Max - Bf) < ?float_error ->
		60 * (Rf - Gf)/(Max - Min) + 240;
	    true -> 
	    0
	end,
    L   = (Max + Min)/2,
    S   = if
	    abs(Max - Min) < ?float_error ->
		0;
	    L > 0.5 ->
		(Max - Min)/(2 - (Max + Min));
	    true ->
		(Max - Min)/(Max + Min)
	end,
    {H, S, L, A}.



