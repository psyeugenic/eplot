%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    egd_heatmap.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-07-25
%%

-module(egd_heatmap).

-export([
	graph/1
    ]).


-record(data, {
	image,
	font,
	width  = 0,
	height = 0,
	vs = [],
	ks = []
    }).

%% graph(Vs) -> binary()
%%     where:
%% Data :: [{term(),term(),Value :: number()}]

graph(Data) ->
    graph(Data, [{width, 300}, {height, 300}]).

graph(Data, _Opts) ->
    S1 = process_data(Data, #data{ vs=gb_trees:empty() }),
    S2 = flatten_data(S1), % gb_trees to list of lists
    S3 = normalize_data(S2),
    S4 = S3,
    %S4 = bucket_data(4,S3),

    W  = 1280,
    H  = 1280,
    Im = egd:create(W,H),
    FontFile = filename:join([code:priv_dir(percept), "fonts", "6x11_latin1.wingsfont"]),
    Font = egd_font:load(FontFile),
 
    render(S4#data{
	    image=Im,
	    font=Font,
	    width=W,
	    height=H
	}),

    Png = egd:render(Im, png, [{render_engine, opaque}]),
    egd:destroy(Im),
    try erlang:exit(Im, normal) catch _:_ -> ok end,
    Png.

render(#data{image=Im,vs=Vs,width=W,height=H}=S) ->
    N    = length(Vs),
    X0   = 50,
    Y0   = 50,
    BbxW = W - X0*2,
    BbxH = H - Y0*2,
    Xm   = BbxW / N,
    Ym   = BbxH / N,
    Clr  = egd:color({137,137,220}),
    egd:filledRectangle(Im, {X0,Y0}, {X0 + BbxW,Y0 + BbxH}, Clr),
    render_y(S,X0,Y0,Xm,Ym,Vs),
    render_labels(S,X0,Y0,Xm,Ym,Vs),
    ok.

render_labels(_,_,_,_,_,[]) -> ok;
render_labels(#data{image=Im,font=Font},X0,Y,Xm,Ym,[{_P1,Fs}|_]) ->
    Black = egd:color({0,0,0}),
    lists:foldl(fun
	    ({P2,_}, {X, up}) ->
		Label = lists:flatten(io_lib:format("~p", [P2])),
		egd:text(Im,{trunc(X),8},Font,Label,Black),
		{X + Xm, down};
	    ({P2,_}, {X, down}) ->
		Label = lists:flatten(io_lib:format("~p", [P2])),
		egd:text(Im,{trunc(X),28},Font,Label,Black),
		{X + Xm, up}
	end, {X0,up}, Fs),
    ok.

render_y(_,_,_,_,_,[]) -> ok;
render_y(#data{image=Im,font=Font}=S,X0,Y,Xm,Ym,[{P1,Fs}|Vs]) ->
    Label = lists:flatten(io_lib:format("~p", [P1])),
    egd:text(Im,{8,trunc(Y)},Font,Label,egd:color({0,0,0})),
    render_x(S,X0,Y,Xm,Ym,Fs),
    render_y(S,X0,Y+Ym,Xm,Ym,Vs).

render_x(_,_,_,_,_,[]) -> ok;
render_x(S,X,Y,Xm,Ym,[{_P2, V}|Fs]) ->
    render_point(S,X,Y,Xm,Ym,V),
    render_x(S,X+Xm,Y,Xm,Ym,Fs).

render_point(#data{image=Im},X,Y,Xm,Ym,V) ->
    %Color = case V of
    %    none -> egd:color({0,0,0});
    %    _    -> egd:color({255,255*(1.0 - V),255*(1.0 - V)})
    %end,
    Color = egd:color({255,255*(1.0 - V),255*(1.0 - V)}),
    Pt1 = {trunc(X)+1, trunc(Y)+1},
    Pt2 = {trunc(X + Xm)-1, trunc(Y + Ym)-1},
    egd:filledRectangle(Im, Pt1, Pt2, Color),
    ok.
 
%bucket_data(Buckets,#data{vs=Vs}=S) ->
%    Limit = 1/Buckets,
%    Vs1 = [{F,[{T,bucket_value(V,Limit)}||{T,V}<-Fs]}||{F,Fs}<-Vs],
%    S#data{vs=Vs1}.
%bucket_value(V,Limit) -> 
%    case bucket_value(V,0.0,Limit) of
%	L when L > 1 -> 1.0;
%	L when L < 0 -> 0.0;
%	L -> L
%    end.
%bucket_value(V,L,Li) when V > L ->
%    bucket_value(V,L+Li,Li);
%bucket_value(_,L,Li) -> L - Li.

normalize_data(#data{vs=Vs}=S) ->
    Max = lists:max([lists:max([V||{_,V}<-Fs])||{_,Fs}<-Vs]),
    S#data{vs=[{F,[{T, math:sqrt(V/Max)}||{T,V}<-Fs]}||{F,Fs}<-Vs]}.

flatten_data(#data{vs=Vs}=S) ->
   S#data{vs=[{K,gb_trees:to_list(Fs)}||{K,Fs}<-gb_trees:to_list(Vs)]}.

process_data(Ds, S) ->
    process_data(directed, Ds, S).
process_data(_,[],#data{vs=Vs,ks=Ks}=S) ->
    Ks1 = lists:usort(Ks),
    %% ensure all entries
    Vs1 = lists:foldl(fun(K1,Vi) ->
		Fs = case gb_trees:lookup(K1,Vi) of
		    {value, Fs0} -> Fs0;
		    none         -> gb_trees:empty()
		end,
		Fs1 = lists:foldl(fun(K2,Fi) ->
			    case gb_trees:is_defined(K2, Fi) of
				true  -> Fi;
				false -> gb_trees:enter(K2, 0.0, Fi)
			    end
		    end, Fs, Ks1),
		gb_trees:enter(K1, Fs1, Vi)
	end, Vs, Ks),
    S#data{ks=Ks1,vs=Vs1};
process_data(directed,[{Tx,Ty,V}|Ds], #data{ks=Ks}=S0) ->
    S1 = update_count(Tx,Ty,V,S0),
    process_data(directed,Ds,S1#data{ks=[Tx,Ty|Ks]});
process_data(Type,[{Tx,Ty,V}|Ds], #data{ks=Ks}=S0) ->
    S1 = update_count(Tx,Ty,V,S0),
    S2 = update_count(Ty,Tx,V,S1),
    process_data(Type,Ds,S2#data{ks=[Tx,Ty|Ks]}).

update_count(From,To,V,#data{vs=Fs}=S) ->
    Ts1 = case gb_trees:lookup(From, Fs) of
	none ->
	    gb_trees:enter(To, V, gb_trees:empty());
	{value, Ts} ->
	    V1 = case gb_trees:lookup(To, Ts) of
		none -> V;
		{value, V0} -> V0 + V
	    end,
	    gb_trees:enter(To, V1, Ts)
    end,
    S#data{ vs = gb_trees:enter(From, Ts1, Fs) }.
