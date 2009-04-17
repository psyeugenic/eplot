-module(eplot_main).
-export([png/3, eview/2]).

png(Inputs, Output, Options0) ->
    Options = merge_options(Options0, get_config()),
    Data0   = parse_data_files(Inputs),
    Data    = case proplists:is_defined(speedup, Options) of
	true  -> data_speedup(Data0);
	false -> Data0
    end,
    B       = graph_binary(proplists:get_value(plot, Options), Data, Options),
    egd:save(B, Output),
    ok.

eview(Inputs, Options0) ->
    Options = proplists:delete(type, merge_options(Options0, get_config())),
    Data0   = parse_data_files(Inputs),
    Data    = case proplists:is_defined(speedup, Options) of
	true  -> data_speedup(Data0);
	false -> Data0
    end,
    W       = proplists:get_value(width, Options),
    H       = proplists:get_value(height, Options),
    P       = eview:start({W,H}),
    B       = graph_binary(proplists:get_value(plot, Options), Data, [{type, raw_bitmap}] ++ Options),
    P ! {self(), bmp_image, B, {W,H}},
    receive {P, done} -> ok end.


graph_binary(plot2d,Data, Options) ->
    egd_chart:graph(Data, Options);
graph_binary(bar2d, Data, Options) ->
    egd_chart:bar2d(Data, Options);
graph_binary(Type, _, _) -> io:format("Bad engine: ~p~n", [Type]), exit(unnormal).

parse_data_files(Filenames) -> parse_data_files(Filenames, []).
parse_data_files([], Out) -> lists:reverse(Out);
parse_data_files([Filename|Filenames], Out) ->
    Data = parse_data_file(Filename),
    parse_data_files(Filenames, [{Filename, Data}|Out]).


merge_options([], Out) -> Out;
merge_options([{Key, Value}|Opts], Out) ->
    case proplists:get_value(Key, Out) of
	undefined -> merge_options(Opts, [{Key, Value}|Out]);
	_         -> merge_options(Opts, [{Key, Value}|proplists:delete(Key, Out)])
    end.

data_speedup([]) -> [];
data_speedup([{Filename,[{X,Y}|T]}|Data]) -> 
    Speedup = data_speedup(T, Y, [{X,1}]),
    [{Filename, Speedup}|data_speedup(Data)].


data_speedup([], _, Out) -> lists:reverse(Out);
data_speedup([{X,Y}|T], F, Out) -> data_speedup(T, F, [{X,F/Y}|Out]).

parse_data_file(Filename) ->
    {ok, Fd} = file:open(Filename, [read]),
    parse_data_file(Fd, io:get_line(Fd, ""), []).

parse_data_file(Fd, eof, Out) -> file:close(Fd), lists:reverse(Out);
parse_data_file(Fd, String, Out) ->
    % expected string is 'number()<whitespace(s)>number()'
    Tokens = string:tokens(String, " \t\n\r"),
    Item = tokens2item(Tokens),
    parse_data_file(Fd, io:get_line(Fd, ""), [Item|Out]).
    
tokens2item(Tokens) ->
    Is = lists:map(fun
	(String) ->
	    list_to_term(String)
	end, Tokens),
    [X,Y|_] = Is,
    {X,Y}.

list_to_term(String) ->
    case catch list_to_number(String) of
	{'EXIT', _} -> list_to_atom(String);
	I -> I
    end.

list_to_number(String) ->
    case catch list_to_integer(String) of
	I when is_integer(I) -> I;
	_ -> list_to_float(String)
    end.

get_config() ->
    Home = os:cmd("echo -n $HOME"),
    Path = filename:join([Home, ".eplot"]),
    File = filename:join([Path, "eplot.config"]),
    case file:consult(File) of
	{ok, Terms} -> Terms;
	{error, enoent} -> make_config(Path, File)
    end.

make_config(Path, File) ->
    Defaults = [{width, 1024}, {height, 800}],
    try
	file:make_dir(Path),
    	{ok, Fd} = file:open(File, [write]),
    	[io:format(Fd, "~p.~n", [Opt]) || Opt <- Defaults],
    	file:close(Fd),
	Defaults
    catch
	A:B ->
	    io:format("Error writing config. ~p ~p~n", [A,B]),
	    Defaults
    end.
    
    
