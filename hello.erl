-module(hello).

-include_lib("wx/include/wx.hrl").
-include_lib("wx/include/gl.hrl").

-export([start/0]).

-define(WIDTH,  640).
-define(HEIGHT, 480).

-define(ZERO, 0.0).
-define(ONE,  1.0).

-record(state, {enabled=true, gl, rot=0, list, file, font20, font10}).

-define(CHARS, [{32,256},    %% ISO_latin
		{1490,1515}, %% Hebrew
		{26084,26090},{26400,26415}, {35480,35500}]). %% Japanese

start() ->
    spawn_link(fun() -> init() end).

init() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, ?wxID_ANY, "wxGLFontServer demo",
			[{pos, {0, 0}}, {size, {?WIDTH, ?HEIGHT}},
			 {style, ?wxDEFAULT_FRAME_STYLE}]),
    
    Opts = [{size, {?WIDTH, ?HEIGHT}}],
    GLAttrib = [{attribList, [?WX_GL_RGBA, ?WX_GL_DOUBLEBUFFER,
			      ?WX_GL_DEPTH_SIZE, 24, 0]}],
    GL = wxGLCanvas:new(Frame, Opts ++ GLAttrib),

    wxFrame:connect(Frame, enter_window),
    wxFrame:connect(Frame, close_window),
    wxFrame:connect(Frame, show),
    %% wxFrame:connect(Frame, key_up),
    wxFrame:connect(GL,    left_down),
    wxFrame:connect(GL,    mousewheel),
    wxFrame:connect(GL,    motion),
    
    wxFrame:show(Frame),
    receive #wx{event=#wxShow{}} -> ok end,

    wxGLCanvas:setCurrent(GL),
    Ver = gl:getString(?GL_VERSION),
    Ven = gl:getString(?GL_VENDOR), 
    io:format("GL: ~s ~s~n", [Ven, Ver]),

    Font = wxFont:new(20, ?wxFONTFAMILY_SWISS, ?wxFONTSTYLE_NORMAL,
		      ?wxFONTWEIGHT_NORMAL),
    {ok, GLFont} = wx_glfont:load_font(Font, []),
    Fixed = wxFont:new(10, ?wxFONTFAMILY_MODERN, ?wxFONTSTYLE_NORMAL,
		       ?wxFONTWEIGHT_NORMAL),
    {ok, GLFixed} = wx_glfont:load_font(Fixed, [{range, ?CHARS}]),

    List = wx_glfont:render_to_list(GLFont, "Hello world!"),
    {ok, File0} = file:read_file(?MODULE_STRING ++ ".erl"),
    %% File = re:split(File0, "\r?\n", [{return, list}]),
    gl:shadeModel(?GL_SMOOTH),
    gl:enable(?GL_BLEND),

    gl:depthFunc(?GL_LEQUAL),
    gl:enable(?GL_DEPTH_TEST),
    gl:clearColor(1.0,1.0,1.0,1.0),

    gl:matrixMode(?GL_PROJECTION),
    gl:loadIdentity(),

    glu:perspective(50, ?WIDTH/?HEIGHT, 0.1, 1000),
    glu:lookAt(0, 0, 3.14,
	       0, 0, -3.14,
	       0, 1, 0),
    set_model_view(),

    State = #state{gl=GL, list=List, file=binary_to_list(File0),
		   font20=GLFont, font10=GLFixed},
    wxFrame:connect(GL, right_up),
    loop(State).
    
loop(State) ->
    receive
	#wx{event=#wxClose{}} ->
	    wx:destroy();
	#wx{event=#wxMouse{type=right_up}} ->
	    New = get_font(State),
	    loop(New)
    after 0 ->
	    wx:batch(fun() -> hello(State) end),
	    wxGLCanvas:swapBuffers(State#state.gl),
	    NRot = (State#state.rot + 2) rem 360,
	    loop(State#state{rot=NRot})
    end.

hello(State) ->
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),

    gl:enable(?GL_LIGHT0),
    gl:enable(?GL_LIGHTING),
    gl:enable(?GL_COLOR_MATERIAL),

    gl:disable(?GL_DEPTH_TEST),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_BLEND),
    gl:enable(?GL_TEXTURE_2D),

    test1(State),

    gl:disable(?GL_LIGHTING),
    test2(State),

    test3(State),
    
    gl:enable(?GL_DEPTH_TEST),
    gl:disable(?GL_TEXTURE_2D),
    gl:disable(?GL_BLEND).

test1(State = #state{font20=Font}) ->
    set_model_view(),
    gl:rotatef(State#state.rot, 1, 0, 0),
    gl:translatef(-1.0, -0.5, 0),

    String = "Erlang r0cks !",
    Size = wx_glfont:text_size(Font, String),
    scale(Size),
    gl:color3ub(0, 255, 0),
    wx_glfont:render(Font, String).

test2(State) ->
    set_model_view(),
    gl:rotatef(-45, 0, 1, 0),
    gl:translatef(-1.0, -0.5, 0),

    {Size, List} = State#state.list,
    scale(Size),

    gl:color3ub(0, 0, 255),
    gl:callList(List).    

set_model_view() ->
    gl:matrixMode(?GL_MODELVIEW),
    gl:loadIdentity().

scale({Width, Height}) ->
    gl:scalef(2.0/Width, 1.0/Height, 1.0).
    
test3(#state{gl=GL, file=File, font10=Font}) ->
    %%gl:color3ub(255, 255, 255),
    gl:color3ub(0, 0, 0),
    {W,H} = wxWindow:getSize(GL),
    gl:viewport(0,0,W,H),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    TextH = float(wx_glfont:height(Font)),
    gl:translatef(30.0, H-30.0-TextH, 0.0),
    wx_glfont:render(Font, "This is a fixed font in size 10"),
    gl:translatef(0.0, -TextH, 0.0),
    wx_glfont:render(Font, "iiiiiiiiiiiiiiiiiiiiiiiiiiiiiii"),
    gl:translatef(0.0, -TextH, 0.0),
    wx_glfont:render(Font, [72,101,98,114,101,119,32,32,32,32,
			    [1513,1500,1493,1501],
			    32,45,45,32,74,97,112,
			    <<97:8,110:8,101:8,115:8,101:8,32:8,40:8>>,
			    [26085,26412,35486],41,10]),

    gl:translatef(0.0, -5*TextH, 0.0),
    wx_glfont:render(Font, File),
    gl:popMatrix(),
    gl:matrixMode(?GL_PROJECTION),
    gl:popMatrix(),
    gl:matrixMode(?GL_MODELVIEW).


get_font(State) ->
    Dlg = wxFontDialog:new(State#state.gl, wxFontData:new()),
    New = case wxFontDialog:showModal(Dlg) of
	      ?wxID_OK ->
		  Font = wxFontData:getChosenFont(wxFontDialog:getFontData(Dlg)),
		  FontDesc = wxFont:getNativeFontInfoUserDesc(Font),
		  io:format("Selected ~s~n",[FontDesc]),
		  {ok, GLFixed} = wx_glfont:load_font(Font, [{range, ?CHARS}]),
		  State#state{font10=GLFixed};
	      _ ->
		  State
	  end,
    wxFontDialog:destroy(Dlg),
    New.
