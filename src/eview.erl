-module(eview).

-include("wx.hrl"). 

-export([start/0, start/1]).

-record(s,  {f,w, bin, bin_size, bmp, image, notify}).

start() -> start({1024,800}).
start(Size) ->
    spawn_link(fun() -> init(Size) end).

init(Size) ->
    P     = wx:new(),
    F     = wxFrame:new(P,1, "eView", [{size, Size}]),
    Opts  = [{size, {1024, 800}}, {style, ?wxSUNKEN_BORDER}],
    W     = wxWindow:new(F, ?wxID_ANY, Opts),
    wxFrame:connect(F, close_window, [{skip,true}]),
    wxWindow:connect(W, paint, [{skip, true}]),
    wxFrame:show(F),
    wxFrame:centre(F),
    loop(#s{ f = F, w = W}).
		
loop(S) ->
    receive 
	_E=#wx{event=#wxPaint{}} ->
	    redraw(S),
	    loop(S);
	E=#wx{id=EXIT, event=EV} when EXIT =:= ?wxID_EXIT; is_record(EV, wxClose) ->
	    catch wxWindow:'Destroy'(S#s.f),
	    S#s.notify ! {self(), done},
	    ok;
	{Pid, bmp_image, Bin, {W,H}} ->
	    S1 = bin2bmp(S#s{bin = Bin, bin_size = {W,H}}),
	    wxWindow:setClientSize(S#s.w, W, H),
	    redraw(S1),
	    wxWindow:setFocus(S#s.w),
	    loop(S1#s{ notify = Pid});
	_ ->
	    loop(S)
    end.

bin2bmp(State) ->
    {W,H} = State#s.bin_size,
    Image = wxImage:new(W,H,State#s.bin),
    Bmp   = wxBitmap:new(Image),
    State#s{ bmp = Bmp, image = Image}.
    

redraw(#s{ w = Win, bmp = undefined}) ->
    DC0  = wxClientDC:new(Win),
    DC = wxBufferedDC:new(DC0),
    wxDC:clear(DC),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok;
redraw(#s{ w = Win, bmp = Bmp}) ->
    DC0  = wxClientDC:new(Win),
    DC = wxBufferedDC:new(DC0),
    wxDC:clear(DC),
    wxDC:drawBitmap(DC,Bmp, {0,0}),
    wxBufferedDC:destroy(DC),
    wxClientDC:destroy(DC0),
    ok.


