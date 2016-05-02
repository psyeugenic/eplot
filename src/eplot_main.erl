-module(eplot_main).
-export([png/3, view/2]).

png(Inputs, Output, Options0) ->
    Options = merge_options(Options0, get_config()),
    Data    = process_data(parse_data_files(Inputs), Options),
    B       = graph_binary(proplists:get_value(plot, Options), Data, Options),
    egd:save(B, Output),
    ok.

view(Inputs, Options0) ->
    Options = proplists:delete(type, merge_options(Options0, get_config())),
    Data    = process_data(parse_data_files(Inputs), Options),
    W       = proplists:get_value(width, Options),
    H       = proplists:get_value(height, Options),
    P       = eplot_view:start({W,H}),
    B       = graph_binary(proplists:get_value(plot, Options), Data, [{type, raw_bitmap}] ++ Options),
    P ! {self(), bmp_image, B, {W,H}},
    receive {P, done} -> ok end.

process_data(Data0, Options) ->
    Data1 = case proplists:is_defined(speedup, Options) of
	true  -> data_speedup(Data0);
	false -> Data0
    end,
    case proplists:is_defined(norm, Options) of
	true -> 
	    [{_Name,Norm}] = parse_data_files([proplists:get_value(norm, Options)]),
	    normalize_data(Data1, Norm);
	false ->
	    Data1
    end.

normalize_data([], _) -> [];
normalize_data([{Name,Data}|Datas], Norm) ->
    [{Name, normalize_data_entries(Name, Data, Norm)}|normalize_data(Datas, Norm)].
normalize_data_entries(_, [], _) -> [];
normalize_data_entries(Name, [{X,Y}|Entries], Norm) ->
    case proplists:get_value(X, Norm) of
	undefined ->
	    io:format(standard_error, "Warning: entry ~p not in data file ~p\r\n", [X, Name]),
	    normalize_data_entries(Name, Entries, Norm);
	Value ->
	    [{X, Y/Value}|normalize_data_entries(Name, Entries, Norm)]
    end.

graph_binary(plot2d,Data, Options) ->
    egd_chart:graph(Data, Options);
graph_binary(bar2d, Data, Options) ->
    egd_chart:bar2d(Data, Options);
graph_binary(Type, _, _) ->
    io:format("Bad engine: ~p~n", [Type]),
    exit(bad_plot_engine).

parse_data_files([]) -> [];
parse_data_files([Filename|Filenames]) ->
    Data = parse_data_file(Filename),
    Name = filename:basename(Filename),
    [{Name, Data}|parse_data_files(Filenames)].


merge_options([], Out) -> Out;
merge_options([{Key, Value}|Opts], Out) ->
    merge_options(Opts, [{Key, Value}|proplists:delete(Key, Out)]).

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
    case lists:map(fun (String) -> string_to_term(String) end, Tokens) of
	[X,Y] -> {X,Y};
	[X,Y,E|_] -> {X,Y,E}
    end.

string_to_term(Value) ->
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


get_config() ->
    Home = os:getenv("HOME"),
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
