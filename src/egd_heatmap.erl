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

    Im = egd:create(1280,1280),
    lists:foreach(fun({F,Ts}) ->
		lists:foreach(fun({T,V}) ->
			    Color = egd:color({255,255*(1.0 - V),255*(1.0 - V)}),
			    Pt1 = {F*10  , T*10},
			    Pt2 = {F*10+9, T*19+9},
			    egd:filledRectangle(Im, Pt1, Pt2, Color)
		    end, Ts)
	end, S4#data.vs),



    %FontFile = filename:join([code:priv_dir(percept), "fonts", "6x11_latin1.wingsfont"]),
    %Font = egd_font:load(FontFile),
 
    Png = egd:render(Im, png, [{render_engine, opaque}]),
    egd:destroy(Im),
    try erlang:exit(Im, normal) catch _:_ -> ok end,
    Png.

bucket_data(Buckets,#data{vs=Vs}=S) ->
    Limit = 1/Buckets,
    Vs1 = [{F,[{T,bucket_value(V,Limit)}||{T,V}<-Fs]}||{F,Fs}<-Vs],
    S#data{vs=Vs1}.

bucket_value(V,Limit) -> 
    case bucket_value(V,0.0,Limit) of
	L when L > 1 -> 1.0;
	L when L < 0 -> 0.0;
	L -> L
    end.
bucket_value(V,L,Li) when V > L ->
    bucket_value(V,L+Li,Li);
bucket_value(_,L,Li) -> L - Li.



normalize_data(#data{vs=Vs}=S) ->
    Max = lists:max([lists:max([V||{_,V}<-Fs])||{_,Fs}<-Vs]),
    io:format("max: ~w~n", [Max]),
    S#data{vs=[{F,[{T, V/Max}||{T,V}<-Fs]}||{F,Fs}<-Vs]}.



flatten_data(#data{vs=Vs}=S) ->
   S#data{vs=[{K,gb_trees:to_list(Fs)}||{K,Fs}<-gb_trees:to_list(Vs)]}.

process_data(Ds, S) ->
    process_data(directed, Ds, S).
process_data(_,[],#data{vs=Vs,ks=Ks}=S) ->
    Ks1 = lists:usort(Ks),
    %% ensure all entries
    Vs1 = lists:foldl(fun(K,Vi) ->
		case gb_trees:lookup(K,Vi) of
		    {value, Ts} -> case gb_trees:is_defined(K, Ts) of
			    true  -> Vi;
			    false -> gb_trees:enter(K, gb_trees:enter(K, 0, Ts), Vi)
			end;
		    none ->
			gb_trees:enter(K, gb_trees:enter(K, 0, gb_trees:empty()), Vi)
		end
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
