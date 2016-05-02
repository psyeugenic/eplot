%%
%% Copyright (C) 2016 Björn-Egil Dahlberg
%%
%% File:    eplot.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2012-11-22
%%

-module(eplot).

-export([main/1]).
-mode(compile).

usage() ->
    Text = 
	"eplot [options] files\r\n"
	"Options:\r\n"
	"\t-o Outfile,           :: filename(), directs the graph to the outfile\r\n"
	"\t-render_engine Engine :: alpha | opaque, type of render engine\r\n"
	"\t-type Type            :: png | raw_bitmap, output type\r\n"
	"\t-plot Plot            :: plot2d | bar2d, plot type\r\n"
	"\t-norm File            :: filename(), normalize against\r\n"
	"\t-width Width          :: integer(), Width\r\n"
	"\t-height Height        :: integer(), Height\r\n"
	"\t-x_label Label        :: string(), X-axis label\r\n"
	"\t-y_label Label        :: string(), Y-axis label\r\n"

	"\t-x_range_min Min      :: number()\r\n"
	"\t-x_range_max Max      :: number()\r\n"
	"\t-y_range_min Min      :: number()\r\n"
	"\t-y_range_max Max      :: number()\r\n"
	"\t-margin Margin        :: number()\r\n"
	"\t-x_ticksize Size      :: number()\r\n"
	"\t-y_ticksize Size      :: number()\r\n"
	"\t-bg_rgba RGB[A]       :: Background, ex. fefefe[fe]\r\n",
    io:format("~s~n", [Text]),
    halt(),
    ok.

main([]) -> usage();
main(Args) ->
    Options = parse_args(Args),
    code:add_patha(ebin()),
    Input = proplists:get_value(input, Options, []),
    case proplists:get_value(o, Options) of
	undefined -> eplot_main:view(Input, Options);
	Output    -> eplot_main:png(Input, Output, Options)
    end.

parse_args(Args) -> 
    parse_args(Args, [{bg_rgba, {255,255,255,255}},{plot, plot2d}], []).
parse_args([], Opts, Inputs) -> [{input, Inputs}|Opts];
parse_args([V0,V1|Args], Opts, Inputs) ->
    case V0 of
	"-h" -> 
	    usage();
	"-speedup" ->
	    parse_args([V1|Args], [{speedup,true}|Opts], Inputs);
	"-bg_rgba" ->
	    parse_args(Args, [{bg_rgba, hexstring2rgb(V1)}|proplists:delete(bg_rgba, Opts)], Inputs);
	"-" ++ SKey->
	    Key = list_to_atom(SKey),
	    parse_args(Args, [{Key, string_to_value(V1)}|proplists:delete(Key, Opts)], Inputs);
	_ ->
	    parse_args([V1|Args], Opts, [V0|Inputs])
    end;
parse_args([Value|Args], Opts, Inputs) ->
    case Value of
	"-h" -> 
	    usage();
	_ ->
	    parse_args(Args, Opts, [Value|Inputs])
    end.

hexstring2rgb(Hs = [_R0,_R1,_G0,_G1,_B0,_B1]) -> hexstring2rgb(Hs ++ "FF");
hexstring2rgb([R0,R1,G0,G1,B0,B1,A0,A1]) ->
    R = hex2byte(R0)*15 + hex2byte(R1),
    G = hex2byte(G0)*15 + hex2byte(G1),
    B = hex2byte(B0)*15 + hex2byte(B1),
    A = hex2byte(A0)*15 + hex2byte(A1),
    {R,G,B,A};
hexstring2rgb(_) -> {255,255,255,255}.

hex2byte(H) when H >= 48, H =<  57 -> H - 48;
hex2byte(H) when H >= 65, H =<  70 -> H - 65 + 10; 
hex2byte(H) when H >= 97, H =< 102 -> H - 97 + 10;
hex2byte(_) -> 0.

string_to_value(Value) ->
    try
	list_to_integer(Value)
    catch
	_:_ ->
	    try
		list_to_float(Value)
	    catch
		_:_ ->
		    list_to_atom(Value)
	    end
    end.

ebin() ->
    Sname = escript:script_name(),
    Rname = real_name(Sname),
    filename:join([filename:dirname(Rname),"..","ebin"]).

real_name(Sname) -> real_name(Sname, file:read_link(Sname)).
real_name(_, {ok, Rname}) -> real_name(Rname, file:read_link(Rname));
real_name(Rname, {error, einval}) -> Rname.
