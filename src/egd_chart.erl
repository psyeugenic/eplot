-module(egd_chart).

-export([
	graph/1, 
	graph/2, 
	bar2d/1,
	bar2d/2
	]).

-export([
	hsl2rgb/1, 
	rgb2hsl/1
	]).

-export([smart_ticksize/3]).

-record(chart, {
	type = png,
	render_engine = opaque,
	margin = 30,			% margin
	bbx = {{30,30}, {130,130}},	% Graph boundingbox (internal)
	ibbx = undefined,
	ranges = {{0,0}, {100,100}},    % Data boundingbox
	width = 160,			% Total chart width
	height = 160,			% Total chart height
	dxdy = {1.0,1.0},
	ticksize = {10,10},
	precision = {2,2},

	% colors
	bg_rgba     = {230, 230, 255, 255},
	margin_rgba = {255, 255, 255, 255},
	graph_rgba  = [],		% ordered color convention

	% graph specific
	x_label = "X",
	y_label = "Y",
	graph_name_yo = 10,		% Name offset from top
	graph_name_yh = 10,		% Name drop offset
	graph_name_xo = 10,		% Name offset from RHS

	% bar2d specific
	bar_width = 40,
	column_width = 40
	}).

-define(float_error, 0.0000000000000001).


%% color conversions
%% H, hue has the range of [0, 360]
%% S, saturation has the range of [0,1]
%% L, lightness has the range of [0,1]

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

%% graph/1 and graph/2
%% In:
%%	Data :: [{Graphname :: atom() | string(), [{X,Y}]}]
%%	Options :: [
%%		% Metric options
%%		{ width,    integer() }, (300)
%%		{ height,   integer() }, (300)
%%		{ margin,   integer() }, (30)
%%		{ ticksize, { integer(), integer() },
%%		{ x_range,  { float(), float() },
%%		{ y_range,  { float(), float() },
%%		
%%		% Naming options
%%		{ x_label,  string() | atom() },
%%		{ y_label,  string() | atom() },
%%
%%		% Color options
%%		{bg_rgba,    {byte(), byte(), byte()}}		
%%	]
graph(Data) -> graph(Data, [{width, 300}, {height, 300}]).

graph(Data, Options) ->
    Chart = graph_chart(Options,Data),
    Im = egd:create(Chart#chart.width, Chart#chart.height),
    LightBlue = egd:color(Chart#chart.bg_rgba),
    {Pt1, Pt2} = Chart#chart.bbx,
    egd:filledRectangle(Im, Pt1, Pt2, LightBlue), % background

    % Fonts? Check for text enabling

    Font = egd_font:load(filename:join([code:priv_dir(percept), "fonts", "6x11_latin1.wingsfont"])),

  
    draw_graphs(Data, Chart, Im),
    
 
    % estetic crop, necessary?
    {{X0,Y0}, {X1,Y1}} = Chart#chart.bbx,
    W = Chart#chart.width,
    H = Chart#chart.height,
    White = egd:color(Chart#chart.margin_rgba),
    egd:filledRectangle(Im, {0,0}, {X0-1,H}, White),
    egd:filledRectangle(Im, {X1+1,0}, {W,H}, White),
    egd:filledRectangle(Im, {0,0}, {W,Y0-1}, White),
    egd:filledRectangle(Im, {0,Y1+1}, {W,H}, White),

    draw_ticks(Chart, Im, Font),
    
    draw_origo_lines(Chart, Im), 	% draw origo crosshair

    draw_graph_names(Data, Chart, Font, Im),
  
    draw_xlabel(Chart, Im, Font),
    draw_ylabel(Chart, Im, Font),

    Png = egd:render(Im, Chart#chart.type),
    egd:destroy(Im),
    try erlang:exit(Im, normal) catch _:_ -> ok end,
    Png.

graph_chart(Opts, Data) ->

    {{X0,Y0},{X1,Y1}} = proplists:get_value(ranges, Opts, ranges(Data)),
    Type      = proplists:get_value(type,         Opts, png),
    Width     = proplists:get_value(width,        Opts, 600),
    Height    = proplists:get_value(height,       Opts, 600),
    Xlabel    = proplists:get_value(x_label,      Opts, "X"),
    Ylabel    = proplists:get_value(y_label,      Opts, "Y"),
    XrangeMax = proplists:get_value(x_range_max,  Opts, X1),
    XrangeMin = proplists:get_value(x_range_min,  Opts, X0),
    YrangeMax = proplists:get_value(y_range_max,  Opts, Y1),
    YrangeMin = proplists:get_value(y_range_min,  Opts, Y0),
    Ranges    = {{XrangeMin, YrangeMin}, {XrangeMax,YrangeMax}},
    Precision = precision_level(Ranges, 10),
    {TsX,TsY} = smart_ticksize(Ranges, 10),
    XTicksize = proplists:get_value(x_ticksize,   Opts, TsX),
    YTicksize = proplists:get_value(y_ticksize,   Opts, TsY),
    Ticksize  = proplists:get_value(ticksize,     Opts, {XTicksize, YTicksize}),
    Margin    = proplists:get_value(margin,       Opts, 30),
    BGC       = proplists:get_value(bg_rgba,      Opts, {230,230, 255, 255}),

    BBX       = {{Margin, Margin}, {Width - Margin, Height - Margin}},
    DxDy      = update_dxdy(Ranges,BBX),
    
    
    #chart{
	type      = Type,
	width     = Width,
	height    = Height,
	x_label   = Xlabel,
	y_label   = Ylabel,
	ranges    = Ranges,
	precision = Precision,
	ticksize  = Ticksize,
	margin    = Margin,
	bbx       = BBX,
	dxdy      = DxDy,
	bg_rgba   = BGC
    }.

draw_ylabel(Chart, Im, Font) ->
    Label = string(Chart#chart.y_label, 2),
    N = length(Label),
    {Fw,_Fh} = egd_font:size(Font),
    Width = N*Fw,
    {{Xbbx,Ybbx}, {_,_}} = Chart#chart.bbx,
    Pt = {Xbbx - trunc(Width/2), Ybbx - 20},
    egd:text(Im, Pt, Font, Label, egd:color({0,0,0})).

draw_xlabel(Chart, Im, Font) ->
    Label = string(Chart#chart.x_label, 2),
    N = length(Label),
    {Fw,_Fh} = egd_font:size(Font),
    Width = N*Fw,
    {{Xbbxl,_}, {Xbbxr,Ybbx}} = Chart#chart.bbx,
    Xc = trunc((Xbbxr - Xbbxl)/2) + Chart#chart.margin,
    Y  = Ybbx + 20,
    Pt = {Xc - trunc(Width/2), Y},
    egd:text(Im, Pt, Font, Label, egd:color({0,0,0})).


color_scheme(I) ->
    egd:color(hsl2rgb({I*55 rem 360, 0.8, 0.3, 120})).

draw_graphs(Datas, Chart, Im) ->
    draw_graphs(Datas, 0, Chart, Im).
draw_graphs([],_,_,_) -> ok;
draw_graphs([{_, Data}|Datas], ColorIndex, Chart, Im) ->
    Color = color_scheme(ColorIndex),
    % convert data to graph data
    % fewer pass of xy2chart
    GraphData = [xy2chart(Pt, Chart) || Pt <- Data],
    draw_graph(GraphData, Color, Im),
    draw_graphs(Datas, ColorIndex + 1, Chart, Im).
   
draw_graph([], _,_) -> ok;
draw_graph([Pt1,Pt2|Data], Color, Im) ->
    draw_graph_dot(Pt1, Color, Im),
    egd:line(Im, Pt1, Pt2, Color),
    draw_graph([Pt2|Data], Color, Im);

draw_graph([Pt|Data], Color, Im) ->
    draw_graph_dot(Pt, Color, Im),
    draw_graph(Data, Color, Im).

draw_graph_dot({X,Y}, Color, Im) ->
    egd:filledEllipse(Im, {X - 3, Y - 3}, {X + 3, Y + 3}, Color).

%% name and color information


draw_graph_names(Datas, Chart, Font, Im) ->
    draw_graph_names(Datas, 0, Chart, Font, Im, 0, Chart#chart.graph_name_yh).
draw_graph_names([],_,_,_,_,_,_) -> ok;
draw_graph_names([{Name, _}|Datas], ColorIndex, Chart, Font, Im, Yo, Yh) ->
    Color = color_scheme(ColorIndex),
    draw_graph_name_color(Chart, Im, Font, Name, Color, Yo),
    draw_graph_names(Datas, ColorIndex + 1, Chart, Font, Im, Yo + Yh, Yh).

draw_graph_name_color(Chart, Im, Font, Name, Color, Yh) ->
    {{_X0,Y0}, {X1,_Y1}} = Chart#chart.bbx,
    Xo   = Chart#chart.graph_name_xo,
    Yo   = Chart#chart.graph_name_yo,
    Xl   = 50,
    LPt1 = {X1 - Xo - Xl, Y0 + Yo + Yh},
    LPt2 = {X1 - Xo, Y0 + Yo + Yh},

    {Fw,Fh} = egd_font:size(Font),
    Str     = string(Name,2),
    N       = length(Str),
    TPt     = {X1 - 2*Xo - Xl - Fw*N, Y0 + Yo + Yh - trunc(Fh/2) - 3},

    egd:filledRectangle(Im, LPt1, LPt2, Color),
    egd:text(Im, TPt, Font, Str, egd:color({0,0,0})).

%% origo crosshair

draw_origo_lines(Chart, Im) ->
    Black  = egd:color({20,20,20}),
    Black1 = egd:color({50,50,50}),
    {{X0,Y0},{X1,Y1}} = Chart#chart.bbx,
    {X,Y} = xy2chart({0,0}, Chart), 
    if
	X > X0, X < X1, Y > Y0, Y < Y1 -> 
	    egd:filledRectangle(Im, {X0,Y}, {X1,Y}, Black1),
	    egd:filledRectangle(Im, {X,Y0}, {X,Y1}, Black1);
	true -> 
	    ok
    end,
    egd:rectangle(Im, {X0,Y0}, {X1,Y1}, Black),
    ok.

% new ticks

draw_ticks(Chart, Im, Font) ->
    {Xts, Yts} = Chart#chart.ticksize,
    {{Xmin,Ymin}, {Xmax,Ymax}} = Chart#chart.ranges,
    Ys = case Ymin of 
	Ymin when Ymin < 0 -> trunc(Ymin/Yts) * Yts;
	_ -> (trunc(Ymin/Yts) + 1) * Yts
    end,
    Xs = case Xmin of
	Xmin when Xmin < 0 -> trunc(Xmin/Xts) * Xts;
	_ -> (trunc(Xmin/Xts) + 1) * Xts
    end,
    draw_yticks_lp(Im, Chart, Ys, Yts, Ymax, Font),
    draw_xticks_lp(Im, Chart, Xs, Xts, Xmax, Font).

draw_yticks_lp(Im, Chart, Yi, Yts, Ymax, Font) when Yi < Ymax ->
    {_,Y}          = xy2chart({0,Yi}, Chart),
    {{X,_}, _}     = Chart#chart.bbx,
    {_, Precision} = Chart#chart.precision,
    draw_perf_ybar(Im, Chart, Y),
    egd:filledRectangle(Im, {X-2,Y}, {X+2,Y}, egd:color({0,0,0})),
    tick_text(Im, Font, Yi, {X,Y}, Precision, left),
    draw_yticks_lp(Im, Chart, Yi + Yts, Yts, Ymax, Font);
draw_yticks_lp(_,_,_,_,_,_) -> ok.

draw_xticks_lp(Im, Chart, Xi, Xts, Xmax, Font) when Xi < Xmax ->
    {X,_}           = xy2chart({Xi,0}, Chart),
    {_, {_,Y}}      = Chart#chart.bbx,
    { Precision, _} = Chart#chart.precision,
    draw_perf_xbar(Im, Chart, X),
    egd:filledRectangle(Im, {X,Y-2}, {X,Y+2}, egd:color({0,0,0})),
    tick_text(Im, Font, Xi, {X,Y}, Precision, below),
    draw_xticks_lp(Im, Chart, Xi + Xts, Xts, Xmax, Font);
draw_xticks_lp(_,_,_,_,_,_) -> ok.


tick_text(Im, Font, Tick, {X,Y}, Precision, Orientation) ->
    String = string(Tick, Precision),
    L = length(String),
    {Xl,Yl} = egd_font:size(Font),
    PxL = L*Xl,
    {Xo,Yo} = case Orientation of
	above -> {-round(PxL/2), -Yl - 3};
	below -> {-round(PxL/2), 3};
	left  -> {round(-PxL - 4),-round(Yl/2) - 1};
	right -> {3, -round(Yl/2)};
	_ -> throw(tick_text_error)
    end,
    egd:text(Im, {X + Xo,Y + Yo}, Font, String, egd:color({0,0,0})).

% background tick bars, should be drawn with background
    
draw_perf_ybar(Im, Chart, Yi) ->
    Pw = 5,
    Lw = 10,
    {{X0,_},{X1,_}} = Chart#chart.bbx,
    [Xl,Xr] = lists:sort([X0,X1]),
    Color = egd:color({180,180,190}),
    lists:foreach(
	fun(X) ->
    	    egd:filledRectangle(Im, {X,Yi}, {X+Pw, Yi}, Color)
	end, lists:seq(Xl,Xr,Lw)),
    ok.

draw_perf_xbar(Im, Chart, Xi) ->
    Pw = 5,
    Lw = 10,
    {{_,Y0},{_,Y1}} = Chart#chart.bbx,
    [Yu,Yl] = lists:sort([Y0,Y1]),
    Color = egd:color({130,130,130}),
    lists:foreach(
	fun(Y) ->
    	    egd:filledRectangle(Im, {Xi,Y}, {Xi, Y+Pw}, Color)
	end, lists:seq(Yu,Yl,Lw)),
    ok.


%% bar2d/1 and bar2d/2
%% In:
%%	Data :: [{ Datasetname :: string(), [{Keyname :: atom() | string(), number() :: Value}]}]
%%		Datasetname = Name of this dataset (the color name)
%%		Keyname = The name of each grouping
%%	Options :: [{Key, Value}]
%%		Key = bar_width
%%		Key = column_width
%%		Colors?
%% Abstract:
%%	The graph is devided into column where each column have
%%	one or more bars.
%%	Each column is associated with a name.
%%	Each bar may have a secondary name (a key).

bar2d(Data) -> bar2d(Data, [{width, 600}, {height, 600}]).

bar2d(Data0, Options) ->
    {ColorMap, Data} = bar2d_convert_data(Data0),
    Chart = bar2d_chart(Options, Data),
    Im = egd:create(Chart#chart.width, Chart#chart.height),
    LightBlue = egd:color(Chart#chart.bg_rgba),
    {Pt1, Pt2} = Chart#chart.bbx,
    egd:filledRectangle(Im, Pt1, Pt2, LightBlue), % background

    
    % Fonts? Check for text enabling

    Font = egd_font:load(filename:join([code:priv_dir(percept), "fonts", "6x11_latin1.wingsfont"])),

    draw_bar2d_ytick(Im, Chart, Font),

    % Color map texts for sets
    draw_bar2d_set_colormap(Im, Chart, Font, ColorMap),

    % Draw bars
    draw_bar2d_data(Data, Chart, Font, Im),
  
    egd:rectangle(Im, Pt1, Pt2, egd:color({0,0,0})),
    Png = egd:render(Im, Chart#chart.type, [{render_engine, Chart#chart.render_engine}]),
    egd:destroy(Im),
    try erlang:exit(Im, normal) catch _:_ -> ok end,
    Png.
    
% [{Dataset, [{Key, Value}]}] -> [{Key, [{Dataset, Value}]}]
bar2d_convert_data(Data) -> bar2d_convert_data(Data, 0,{[], []}).
bar2d_convert_data([], _, {ColorMap, Out}) -> {lists:reverse(ColorMap), lists:sort(Out)};
bar2d_convert_data([{Set, KVs}|Data], ColorIndex, {ColorMap, Out}) ->
    Color = color_scheme(ColorIndex),
    bar2d_convert_data(Data, ColorIndex + 1, {[{Set,Color}|ColorMap], bar2d_convert_data_kvs(KVs, Set, Color, Out)}).

bar2d_convert_data_kvs([], _,_, Out) -> Out;
bar2d_convert_data_kvs([{Key, Value}|KVs], Set, Color, Out) ->
    case proplists:get_value(Key, Out) of 
	undefined ->
	    bar2d_convert_data_kvs(KVs, Set, Color, [{Key, [{{Color, Set}, Value}]}|Out]);
	DVs ->
	    bar2d_convert_data_kvs(KVs, Set, Color, [{Key, [{{Color, Set}, Value} | DVs]} | proplists:delete(Key, Out)])
    end.
    
% beta color map, static allocated

bar2d_chart(Opts, Data) ->
    Values = lists:foldl(fun
	({_, DVs}, Out) ->
	    Vs = [V || {_,V} <- DVs],
	    Out ++ Vs
	end, [], Data),
    Type     = proplists:get_value(type,          Opts, png),
    Margin   = proplists:get_value(margin,        Opts, 30),
    Width    = proplists:get_value(width,         Opts, 600),
    Height   = proplists:get_value(height,        Opts, 600),
    Xrange   = proplists:get_value(y_range,       Opts, 0),
    Ranges   = proplists:get_value(ranges,        Opts, {{0,0}, {length(Data), lists:max([Xrange|Values])}}),
    Ticksize = proplists:get_value(ticksize,      Opts, smart_ticksize(Ranges, 10)),
    Cw       = proplists:get_value(column_width,  Opts, {ratio, 0.8}),
    Bw       = proplists:get_value(bar_width,     Opts, {ratio, 1.0}),
    InfoW    = proplists:get_value(info_box,      Opts, 0),
    Renderer = proplists:get_value(render_engine, Opts, opaque),
    % colors
    BGC      = proplists:get_value(bg_rgba,       Opts, {230, 230, 255, 255}),
    MGC      = proplists:get_value(margin_rgba,   Opts, {255, 255, 255, 255}),

    % bounding box
    IBBX     = {{Width - Margin - InfoW, Margin}, {Width - Margin, Height - Margin}},
    BBX      = {{Margin, Margin}, {Width - Margin - InfoW - 10, Height - Margin}},
    DxDy     = update_dxdy(Ranges, BBX),

    #chart{
	type            = Type,
	margin	 	= Margin,
	width	 	= Width,
	height	 	= Height,
	ranges   	= Ranges,
	ticksize 	= Ticksize,
	bbx	 	= BBX,
	ibbx            = IBBX,
	dxdy 	 	= DxDy,
	column_width 	= Cw,
	bar_width 	= Bw,
	margin_rgba     = MGC,
	bg_rgba         = BGC,
	render_engine   = Renderer
    }.

draw_bar2d_set_colormap(Im, Chart, Font, ColorMap) ->
    Margin = Chart#chart.margin,
    draw_bar2d_set_colormap(Im, Chart, Font, ColorMap, {Margin, 3}, Margin).

draw_bar2d_set_colormap(_, _, _, [], _, _) -> ok;
draw_bar2d_set_colormap(Im, Chart, Font, [{Set, Color}|ColorMap], {X, Y}, Margin) ->
    String = string(Set, 2),
    egd:text(Im, {X + 10, Y}, Font, String, egd:color({0,0,0})),
    egd:filledRectangle(Im, {X,Y+3}, {X+5, Y+8}, Color),
    draw_bar2d_set_colormap_step(Im, Chart, Font, ColorMap, {X,Y}, Margin).

draw_bar2d_set_colormap_step(Im, Chart, Font, ColorMap, {X,Y}, Margin) when (Y + 23) < Margin ->
    draw_bar2d_set_colormap(Im, Chart, Font, ColorMap, {X, Y + 12}, Margin);
draw_bar2d_set_colormap_step(Im, Chart, Font, ColorMap, {X,_Y}, Margin) ->
    draw_bar2d_set_colormap(Im, Chart, Font, ColorMap, {X + 144, 3}, Margin).

draw_bar2d_ytick(Im, Chart, Font) ->
    {_, Yts}               = Chart#chart.ticksize,
    {{_, _}, {_, Ymax}} = Chart#chart.ranges,
    draw_bar2d_yticks_up(Im, Chart, Yts, Yts, Ymax, Font).   %% UPPER tick points
    
draw_bar2d_yticks_up(Im, Chart, Yi, Yts, Ymax, Font) when Yi < Ymax ->
    {X, Y}         = xy2chart({0,Yi}, Chart),
    {_, Precision} = Chart#chart.precision,
    draw_bar2d_ybar(Im, Chart, Y),
    egd:filledRectangle(Im, {X-2,Y}, {X+2,Y}, egd:color({0,0,0})),
    tick_text(Im, Font, Yi, {X,Y}, Precision, left),
    draw_bar2d_yticks_up(Im, Chart, Yi + Yts, Yts, Ymax, Font);
draw_bar2d_yticks_up(_,_,_,_,_,_) -> ok.

draw_bar2d_ybar(Im, Chart, Yi) ->
    Pw = 5,
    Lw = 10,
    {{X0,_},{X1,_}} = Chart#chart.bbx,
    [Xl,Xr] = lists:sort([X0,X1]),
    Color = egd:color({180,180,190}),
    lists:foreach(
	fun(X) ->
    	    egd:filledRectangle(Im, {X-Pw,Yi}, {X, Yi}, Color)
	end, lists:seq(Xl+Pw,Xr,Lw)),
    ok.

draw_bar2d_data(Columns, Chart, Font, Im) ->
    {{Xl,_}, {Xr,_}} = Chart#chart.bbx,
    Cn = length(Columns),	% number of columns
    Co = (Xr - Xl)/(Cn),	% column offset within chart
    Cx = Xl + Co/2,		% start x of column
    draw_bar2d_data_columns(Columns, Chart, Font, Im, Cx, Co).

draw_bar2d_data_columns([], _, _, _, _, _) -> ok;
draw_bar2d_data_columns([{Name, Bars} | Columns], Chart, Font, Im, Cx, Co) ->
    {{_X0,_Y0}, {_X1,Y1}} = Chart#chart.bbx,

    Cwb = case Chart#chart.column_width of
	default -> Co;
	{ratio, P} when is_number(P) -> P*Co;
	Cw when is_number(Cw) -> lists:min([Cw,Co])
    end,

    %% draw column text
    String = string(Name, 2),
    Ns = length(String),
    {Fw, Fh} = egd_font:size(Font),
    L = Fw*Ns,
    Tpt = {trunc(Cx - L/2 + 2), Y1 + Fh},
    egd:text(Im, Tpt, Font, String, egd:color({0,0,0})),

    Bn = length(Bars),		% number of bars
    Bo = Cwb/Bn,	        % bar offset within column
    Bx = Cx - Cwb/2 + Bo/2,	% starting x of bar

    CS = 43,
    draw_bar2d_data_bars(Bars, Chart, Font, Im, Bx, Bo, CS),
    draw_bar2d_data_columns(Columns, Chart, Font, Im, Cx + Co, Co).

draw_bar2d_data_bars([], _, _, _, _, _, _) -> ok;
draw_bar2d_data_bars([{{Color,_Set}, Value}|Bars], Chart, Font, Im, Bx, Bo,CS) ->
    {{_X0,_Y0}, {_X1,Y1}} = Chart#chart.bbx,
    {_, Precision}     = Chart#chart.precision,
    {_, Y}             = xy2chart({0, Value}, Chart),

    Bwb = case Chart#chart.bar_width of
	default -> Bo;
	{ratio, P} when is_number(P) -> P*Bo;
	Bw when is_number(Bw) -> lists:min([Bw,Bo])
    end,


    Black = egd:color({0,0,0}),

    % draw bar text
    String = string(Value, Precision),
    Ns = length(String),
    {Fw, Fh} = egd_font:size(Font),
    L = Fw*Ns,
    Tpt = {trunc(Bx - L/2 + 2), Y - Fh - 5},
    egd:text(Im, Tpt, Font, String, Black),

   
    Pt1 = {trunc(Bx - Bwb/2), Y},
    Pt2 = {trunc(Bx + Bwb/2), Y1},
    egd:filledRectangle(Im, Pt1, Pt2, Color),
    egd:rectangle(Im, Pt1, Pt2, Black),
    draw_bar2d_data_bars(Bars, Chart, Font, Im, Bx + Bo, Bo, CS + CS).

%%==========================================================================
%%
%%              Aux functions     
%%
%%==========================================================================


xy2chart({X,Y}, Chart) ->
    {{Rx0,Ry0}, {_Rx1,_Ry1}} = Chart#chart.ranges,
    {{Bx0,By0}, {_Bx1,By1}} = Chart#chart.bbx,
    {Dx, Dy} = Chart#chart.dxdy,
    {round(X*Dx + Bx0 - Rx0*Dx), round(By1 - (Y*Dy + By0 - Ry0*Dy - Chart#chart.margin))}. 


ranges([{_Name, Es}|Data]) when is_list(Es) ->
    Ranges = xy_minmax(Es),
    ranges(Data, Ranges).

ranges([], Ranges) -> Ranges;
ranges([{_Name, Es}|Data], CoRanges) when is_list(Es) ->
    Ranges = xy_minmax(Es),
    ranges(Data, xy_resulting_ranges(Ranges, CoRanges)).

    
smart_ticksize({{X0, Y0}, {X1, Y1}}, N) ->
    { smart_ticksize(X0,X1,N), smart_ticksize(Y0,Y1,N)}.


smart_ticksize(S, E, N) when is_number(S), is_number(E), is_number(N) ->
    % Calculate stepsize then 'humanize' the value to a human pleasing format.
    R = abs((E - S))/N,
    if 
	abs(R) < ?float_error -> 2.0;
	true ->
	    % get the ratio on the form of 2-3 significant digits.
	    %V =  2 - math:log10(R),
	    %P = trunc(V + 0.5),
	    P = precision_level(S, E, N),
	    M = math:pow(10, P),
	    Vsig = R*M,
	    %% do magic    
	    Rsig = Vsig/50,
	    Hsig = 50 * trunc(Rsig + 0.5),
	    %% fin magic
	    Hsig/M
    end;
smart_ticksize(_, _, _) -> 2.0.

precision_level({{X0, Y0}, {X1, Y1}}, N) ->
     { precision_level(X0,X1,N), precision_level(Y0,Y1,N)}.

precision_level(S, E, N) when is_number(S), is_number(E) ->
    % Calculate stepsize then 'humanize' the value to a human pleasing format.
    R = abs((E - S))/N,
    if 
	abs(R) < ?float_error -> 2;
	true ->
	    % get the ratio on the form of 2-3 significant digits.
	    V =  2 - math:log10(R),
	    trunc(V + 0.5)
    end;
precision_level(_, _, _) -> 2.

% on form [{X,Y}]
xy_minmax(Elements) ->
    Xs = [ X || {X,_} <- Elements ],
    Ys = [ Y || {_,Y} <- Elements ],
    {{lists:min(Xs),lists:min(Ys)}, {lists:max(Xs), lists:max(Ys)}}.

xy_resulting_ranges({{X0,Y0},{X1,Y1}},{{X2,Y2},{X3,Y3}}) ->
    { 	
	{lists:min([X0,X1,X2,X3]),
	 lists:min([Y0,Y1,Y2,Y3])},
	{lists:max([X0,X1,X2,X3]),
	 lists:max([Y0,Y1,Y2,Y3])}
    }.

update_dxdy({{Rx0, Ry0}, {Rx1, Ry1}}, {{Bx0,By0},{Bx1,By1}}) ->
   Dx = divide((Bx1 - Bx0),(Rx1 - Rx0)),
   Dy = divide((By1 - By0),(Ry1 - Ry0)),
   {Dx,Dy}.

divide(_T,N) when abs(N) < ?float_error -> 0.0;
%divide(T,N) when abs(N) < ?float_error -> exit({bad_divide, {T,N}});
divide(T,N) -> T/N.

%print_info_chart(Chart) ->
%    io:format("Chart ->~n"),
%    io:format("    type:     ~p~n", [Chart#chart.type]),
%    io:format("    margin:   ~p~n", [Chart#chart.margin]),
%    io:format("    bbx:      ~p~n", [Chart#chart.bbx]),
%    io:format("    ticksize: ~p~n", [Chart#chart.ticksize]),
%    io:format("    ranges:   ~p~n", [Chart#chart.ranges]),
%    io:format("    width:    ~p~n", [Chart#chart.width]),
%    io:format("    height:   ~p~n", [Chart#chart.height]),
%    io:format("    dxdy:     ~p~n", [Chart#chart.dxdy]),
%    ok.

string(E, _P) when is_atom(E)    -> atom_to_list(E);
string(E,  P) when is_float(E)   -> float_to_maybe_integer_to_string(E, P); 
string(E, _P) when is_integer(E) -> s("~w", [E]);
string(E, _P) when is_binary(E)  -> lists:flatten(binary_to_list(E));
string(E, _P) when is_list(E)    -> s("~s", [E]).

float_to_maybe_integer_to_string(F, P) ->
    I = trunc(F),
    A = abs(I - F),
    if 
	% integer
	A < ?float_error -> s("~w", [I]);

	true ->
	% float
	    Format = s("~~.~wf", [P]),
	    s(Format, [F])
    end.

s(Format, Terms) -> lists:flatten(io_lib:format(Format, Terms)).

