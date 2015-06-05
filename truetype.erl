%%%-------------------------------------------------------------------
%%% @author Dan Gudmundsson <dgud@erlang.org>
%%% @copyright (C) 2012, Dan Gudmundsson
%%% @doc
%%%   This library processes TrueType files:
%%%       parse files
%%%       extract glyph metrics
%%%       extract glyph shapes
%%%       render glyphs to one-channel bitmaps with antialiasing (box filter)
%%% @end
%%% Created : 27 Aug 2012 by Dan Gudmundsson <dgud@erlang.org>
%%%-------------------------------------------------------------------

%% Heavily inspired from Sean Barret's code @ nothings.org (see stb_truetype.h)
%%
%% @doc
%%      Codepoint
%%         Characters are defined by unicode codepoints, e.g. 65 is
%%         uppercase A, 231 is lowercase c with a cedilla, 0x7e30 is
%%         the hiragana for "ma".
%%
%%      Glyph
%%         A visual character shape (every codepoint is rendered as
%%         some glyph)
%%
%%      Glyph index
%%         A font-specific integer ID representing a glyph
%%
%%      Baseline
%%         Glyph shapes are defined relative to a baseline, which is the
%%         bottom of uppercase characters. Characters extend both above
%%         and below the baseline.
%%
%%      Current Point
%%         As you draw text to the screen, you keep track of a "current point"
%%         which is the origin of each character. The current point's vertical
%%         position is the baseline. Even "baked fonts" use this model.
%%
%%      Vertical Font Metrics
%%         The vertical qualities of the font, used to vertically position
%%         and space the characters. See docs for get_font_v_metrics.
%%
%%      Font Size in Pixels or Points
%%         The preferred interface for specifying font sizes in truetype
%%         is to specify how tall the font's vertical extent should be in pixels.
%%         If that sounds good enough, skip the next paragraph.
%%
%%         Most font APIs instead use "points", which are a common typographic
%%         measurement for describing font size, defined as 72 points per inch.
%%         truetype provides a point API for compatibility. However, true
%%         "per inch" conventions don't make much sense on computer displays
%%         since they different monitors have different number of pixels per
%%         inch. For example, Windows traditionally uses a convention that
%%         there are 96 pixels per inch, thus making 'inch' measurements have
%%         nothing to do with inches, and thus effectively defining a point to
%%         be 1.333 pixels. Additionally, the TrueType font data provides
%%         an explicit scale factor to scale a given font's glyphs to points,
%%         but the author has observed that this scale factor is often wrong
%%         for non-commercial fonts, thus making fonts scaled in points
%%         according to the TrueType spec incoherently sized in practice.
%%
%% ADVANCED USAGE
%%
%%   Quality:
%%
%%    - Use the functions with Subpixel at the end to allow your characters
%%      to have subpixel positioning. Since the font is anti-aliased, not
%%      hinted, this is very import for quality. (This is not possible with
%%      baked fonts.)
%%
%%    - Kerning is now supported, and if you're supporting subpixel rendering
%%      then kerning is worth using to give your text a polished look.
%%
%%

-module(truetype).

-export([init_font/1, init_font/2, font_info/1,
	 %% Font metrics and scale
	 scale_for_pixel_height/2, scale_for_mapping_em_to_pixels/2,
	 get_font_v_metrics/1, get_font_bb/1,
	 %% Glyph info
	 find_glyph_index/2,
	 get_glyph_h_metrics/2, get_glyph_kern_adv/3, get_codepoint_kern_adv/3,
	 get_glyph_box/2, get_glyph_bitmap_box/3, get_glyph_bitmap_box/4,
	 get_glyph_shape/2,
	 %% Bitmap rendering
	 get_codepoint_bitmap/3, get_codepoint_bitmap/4,
	 get_glyph_bitmap/3,get_glyph_bitmap/4,
	 %% Bake font bitmap
	 bake_bitmap/3
	]).

-compile(export_all).

-define(S16, 16/signed).
-define(U16, 16/unsigned).
-define(S32, 32/signed).
-define(U32, 32/unsigned).
-define(SKIP, _/binary).

-record(vertex, {pos, c, type}).
-record(gi, {pos, dim, off, advance, char, glyph, bin}).
-record(bci, {x0,y0,x1,y1,ox,oy,advance}).

-record(ttf_info,
	{num_glyphs,     %% Number of Glyphs

	 %% Offsets to table locations
	 loca, head, glyf, hhea, hmtx, kern,

	 index_map,     %% A Cmap mapping for out  chosen character encoding
	 index_to_loc_format, %% Format needed to map from glyph index to glyph
	 data          %% The binary file
	}).

-type ttf() :: #ttf_info{}.
-type scale() :: Uniform::float() | {ScaleX::float(),ScaleY::float()}.
-type shift() :: Uniform::float() | {ShiftX::float(),ShiftY::float()}.
-type size() :: {Width::integer(), Height::integer()}.
-type vertex() :: #vertex{}.
-type platform() :: unicode | mac | microsoft | integer().
-type encoding() :: unicode | roman | integer().
-type language() :: english | integer().  %% 0 if platform is unicode

%% Initiates (and optionally reads from file) a ttf font.
%%
%% Each .ttf/.ttc file may have more than one font. Each font has a
%% sequential index number starting from 0. A regular .ttf file will
%% only define one font and it always be at index 0.
-spec init_font(Font|FileName) ->
		       {ok, ttf()} | {error, term()}
			   when Font :: binary(),
				FileName :: list().
init_font(Bin) -> init_font(Bin, []).

-spec init_font(Font|FileName, [Option]) ->
		       {ok, ttf()} | {error, term()}
			   when Font :: binary(),
				FileName :: list(),
				Option :: {index, integer()}.
init_font(Bin, Opts) when is_binary(Bin) ->
    init_font_1(Bin, Opts);
init_font(Filename, Opts) ->
    case file:read_file(Filename) of
	{ok, Bin} -> init_font_1(Bin, Opts);
	Error -> Error
    end.

%% Return the requested string from font
%% By default font family and subfamily (if not regular)
-spec font_info(Font::ttf()) -> string().
font_info(Font) ->
    StdInfoItems = [info(1),info(2),info(3),info(4),info(16),info(17)],
    Try = [{StdInfoItems, microsoft, unicode, english},
	   {StdInfoItems, unicode, unicode, 0},
	   {StdInfoItems, mac, roman, english}
	  ],
    font_info_2(Font, Try).

font_info_2(Font, [{Id,Platform,Enc,Lang}|Rest]) ->
    case font_info(Font, Id, Platform, Enc, Lang) of
	[] -> font_info_2(Font, Rest);
	Info -> Info
    end.

%% Return the requested string from font
%% Info Items: 1,2,3,4,16,17 may be interesting
%% Returns a list if the encoding is known otherwise a binary.
%% Return the empty list is no info that could be matched is found.
-spec font_info(Font::ttf(),
		[InfoId::integer()],
		Platform::platform(),
		Encoding::encoding(),
		Language::language()) -> [{InfoId::integer, string()}].
font_info(#ttf_info{data=Bin}, Id, Platform, Encoding, Language) ->
    case find_table(Bin, <<"name">>) of
	false -> [];
	Name ->
	    <<_:Name/binary, _:16, Count:?U16, StringOffset:?U16, FI/binary>> = Bin,
	    <<_:Name/binary, _:StringOffset/binary, Strings/binary>> = Bin,
	    get_font_info(Count, FI, Strings, Id, Platform, Encoding, Language)
    end.

%% Computes a scale factor to produce a font whose "height" is 'pixels' tall.
%% Height is measured as the distance from the highest ascender to the lowest
%% descender; in other words, it's equivalent to calling stbtt_GetFontVMetrics
%% and computing:
%%       scale = pixels / (ascent - descent)
%% so if you prefer to measure height by the ascent only, use a similar calculation.

-spec scale_for_pixel_height(Font::ttf(), PixelHeight::float()) -> Scale::float().
scale_for_pixel_height(#ttf_info{data=Bin, hhea=Hhea}, Size) ->
    <<_:Hhea/binary, _Ver:?U32, Ascent:?S16, Descent:?S16, ?SKIP>> = Bin,
    Size / (Ascent - Descent).

%% Computes a scale factor to produce a font whose EM size is mapped to
%% 'pixels' tall.
-spec scale_for_mapping_em_to_pixels(Font::ttf(), Size::float()) -> Scale::float().
scale_for_mapping_em_to_pixels(#ttf_info{data=Bin, head=Head}, Size) ->
    <<_:Head/binary, _:18/binary, UnitsPerEm:?U16, ?SKIP>> = Bin,
    Size / UnitsPerEm.

%% ascent is the coordinate above the baseline the font extends; descent
%% is the coordinate below the baseline the font extends (i.e. it is typically negative)
%% lineGap is the spacing between one row's descent and the next row's ascent...
%% so you should advance the vertical position by "ascent - descent + lineGap"
%%   these are expressed in unscaled coordinates, so you must multiply by
%%   the scale factor for a given size
-spec get_font_v_metrics(Font::ttf()) ->
				{ Ascent::integer(),
				  Descent::integer(),
				  LineGap::integer()}.
get_font_v_metrics(#ttf_info{data=Bin, hhea=Hhea}) ->
    <<_:Hhea/binary, _:32, Ascent:?S16, Descent:?S16, LineGap:?S16, ?SKIP>> = Bin,
    {Ascent,Descent,LineGap}.

%% the bounding box around all possible characters
-spec get_font_bb(Font::ttf()) ->
			 {X0::integer(),Y0::integer(),
			  X1::integer(),Y1::integer()}.
get_font_bb(#ttf_info{data=Bin, head=Head}) ->
    <<_:Head/binary, _:36/binary, X0:?S16,Y0:?S16,X1:?S16,Y1:?S16, ?SKIP>> = Bin,
    {X0,Y0,X1,Y1}.

%% leftSideBearing is the offset from the current horizontal position
%% to the left edge of the character advanceWidth is the offset from
%% the current horizontal position to the next horizontal position
%% these are expressed in unscaled coordinates
-spec get_glyph_h_metrics(Font::ttf(), Glyph::integer()) ->
				 { Advance::integer(),
				   LeftSideBearing::integer()}.
get_glyph_h_metrics(#ttf_info{data=Bin, hhea=Hhea, hmtx=Hmtx}, Glyph) ->
    <<_:Hhea/binary, _:34/binary, LongHorMetrics:?U16, ?SKIP>> = Bin,
    case Glyph < LongHorMetrics of
	true ->
	    Skip = 4*Glyph,
	    <<_:Hmtx/binary, _:Skip/binary, Advance:?S16, LeftSideBearing:?S16, ?SKIP>> = Bin,
	    {Advance, LeftSideBearing};
	false ->
	    Skip1 = 4*(LongHorMetrics-1),
	    <<_:Hmtx/binary, _:Skip1/binary, Advance:?S16, ?SKIP>> = Bin,
	    Skip2 = 4*LongHorMetrics+2*(Glyph-LongHorMetrics),
	    <<_:Hmtx/binary, _:Skip2/binary, LeftSideBearing:?S16, ?SKIP>> = Bin,
	    {Advance, LeftSideBearing}
    end.

%% an additional amount to add to the 'advance' value between ch1 and ch2
-spec get_codepoint_kern_adv(Font::ttf(), Char1::integer(), Char2::integer()) ->
				    {ExtraAdvance::integer()}.
get_codepoint_kern_adv(#ttf_info{kern=false}, _G1, _G2) -> 0;
get_codepoint_kern_adv(Font=#ttf_info{}, G1, G2) ->
    get_glyph_kern_adv(Font, find_glyph_index(Font,G1), find_glyph_index(Font,G2)).

-spec get_glyph_kern_adv(Font::ttf(), Glyph1::integer(), Glyph2::integer()) ->
				{ExtraAdvance::integer()}.
get_glyph_kern_adv(#ttf_info{kern=false}, _G1, _G2) ->
    %% No kerning info
    0;
get_glyph_kern_adv(#ttf_info{data=Bin,kern=Kern}, G1, G2) ->
    <<_:Kern/binary, _:16, Tabs:?U16, _:32, HFlag:?U16, R:?U16, _:64, Info/binary>> = Bin,
    if Tabs < 1 -> 0;    %% Tabs must at least 1
       HFlag /= 1 -> 0;  %% Horizontal flag must be in format
       true ->
	    Needle = (G1 bsl 16) bor G2,
	    glyphs_kern_search(0, R-1, Needle, Info)
    end.

%% Creates a large-enough single-channel 8bpp bitmap and renders the
%% specified character/glyph at the specified scale into it, with
%% antialiasing. 0 is no coverage (transparent), 255 is fully covered (opaque).
%% *width & *height are filled out with the width & height of the bitmap,
%% which is stored left-to-right, top-to-bottom.
%%
%% xoff/yoff are the offset it pixel space from the glyph origin to the top-left of the bitmap
-spec get_codepoint_bitmap(Font::ttf(), Scale::scale(), Char::integer()) ->
				  {Size::size(),
				   Offset::{integer(),integer()},
				   Bitmap::binary()
				  }.
get_codepoint_bitmap(TTF, Scale, CodePoint) ->
    get_codepoint_bitmap(TTF, Scale, 0.0, CodePoint).

%% Same as above but you can specify a subpixel shift for the character
-spec get_codepoint_bitmap(Font::ttf(), Scale::scale(),
			   SubPixel::shift(), Char::integer()) ->
				  {Size::size(),
				   Offset::{integer(),integer()},
				   Bitmap::binary()
				  }.
get_codepoint_bitmap(TTF, Scale, Shift, CodePoint) ->
    get_glyph_bitmap(TTF, Scale, Shift, find_glyph_index(TTF, CodePoint)).

-spec get_glyph_bitmap(Font::ttf(), Scale::scale(), Glyph::integer()) ->
			      {Size::size(),
			       Offset::{integer(),integer()},
			       Bitmap::binary()
			      }.
get_glyph_bitmap(TTF, Scale, Glyph) ->
    get_glyph_bitmap(TTF, Scale, 0.0, Glyph).

-spec get_glyph_bitmap(Font::ttf(), Scale::scale(),
		       SubPixel::shift(), Glyph::integer()) ->
			      {Size::size(),
			       Offset::{integer(),integer()},
			       Bitmap::binary()
			      }.
get_glyph_bitmap(TTF, Scale, Shift, Glyph) ->
    Vertices = get_glyph_shape(TTF, Glyph),
    {X0,Y0,X1,Y1} = get_glyph_bitmap_box(TTF, Scale, Glyph),
    %% io:format("Glyph ~p ~p~n",[Glyph, length(Vertices)]),
    %% io:format("BB ~p ~p ~p ~p~n",[X0,Y0,X1,Y1]),
    %% [io:format(" ~p ~n",[V]) || V <- Vertices],
    W = X1-X0, H = Y1-Y0,
    Bin = rasterize(Vertices, 0.35, Scale, Shift, X0,Y0, true, {W,H}),
    true = W*H =:= byte_size(Bin),
    {{W,H},{X0,Y0},Bin}.

%% Get the bbox of the bitmap centered around the glyph origin; so the
%% bitmap width is ix1-ix0, height is iy1-iy0, and location to place
%% the bitmap top left is (leftSideBearing*scale,iy0).
%% (Note that the bitmap uses y-increases-down, but the shape uses
%%  y-increases-up, so CodepointBitmapBox and CodepointBox are inverted.)
-spec get_glyph_bitmap_box(Font::ttf(), Scale::scale(), Glyph::integer()) ->
				  {X0::integer(),Y0::integer(),
				   X1::integer(),Y1::integer()}.

get_glyph_bitmap_box(TTF, Scale, Glyph) ->
    get_glyph_bitmap_box(TTF, Scale, 0.0, Glyph).

-spec get_glyph_bitmap_box(Font::ttf(), Scale::scale(),
			   SubPixel::shift(), Glyph::integer()) ->
				  {X0::integer(),Y0::integer(),
				   X1::integer(),Y1::integer()}.
get_glyph_bitmap_box(TTF, Scale, Shift, Glyph) ->
    {ScaleX,ScaleY} = scale(Scale),
    {ShiftX,ShiftY} = shift(Shift),
    {X0,Y0,X1,Y1} = get_glyph_box(TTF, Glyph),
    { floor(X0*ScaleX+ShiftX),
     -ceil( Y1*ScaleY+ShiftY),
      ceil( X1*ScaleX+ShiftX),
     -floor(Y0*ScaleY+ShiftY)}.

-spec get_glyph_box(Font::ttf(), Glyph::integer()) ->
			   {X0::integer(),Y0::integer(),
			    X1::integer(),Y1::integer()}.
get_glyph_box(TTF = #ttf_info{data=Bin}, Glyph) ->
    case get_glyf_offset(TTF, Glyph) of
	Offset when Offset > 0  ->
	    <<_:Offset/binary, _:16, X0:?S16, Y0:?S16, X1:?S16, Y1:?S16, ?SKIP>> = Bin,
	    {X0,Y0,X1,Y1};
	_ ->
	    {0,0,0,0}
    end.

-spec get_glyph_shape(Font::ttf(), Glyph::integer()) -> Vertices::vertex().
get_glyph_shape(TTF, Glyph) ->
    get_glyph_shape_impl(TTF, get_glyf_offset(TTF, Glyph)).

%% Converts UnicodeCodePoint to Glyph index
%% Glyph 0 is the undefined glyph
-spec find_glyph_index(Font::ttf(), Char::integer()) -> Glyph::integer().
find_glyph_index(#ttf_info{data=Bin, index_map=IndexMap}, UnicodeCP) ->
    case Bin of
	%% Format0: Apple byte encoding
	<<_:IndexMap/binary, 0:?U16, Bytes:?U16, _:16, _:UnicodeCP/binary, Index:8, ?SKIP>>
	  when UnicodeCP < (Bytes-6) -> Index;
	<<_:IndexMap/binary, 0:?U16, ?SKIP>> -> 0;

	%% Format2: Mixed 8/16 bits mapping for Japanese, Chinese and Korean
	<<_:IndexMap/binary, 2:?U16, ?SKIP>> -> 0; %% TODO

	%% Format4: 16 bit mapping
	<<_:IndexMap/binary, 4:?U16, _Len:16, _Lan:16, Format4/binary>> ->
	    format_4_index(Format4, UnicodeCP);

	%% Format6: Dense 16 bit mapping
	<<_:IndexMap/binary, 6:?U16, _Len:16, _Lang:16,
	  First:?U16, Count:?U16, IndexArray/binary>> ->
	    case UnicodeCP >= First andalso UnicodeCP < (First+Count) of
		false -> 0;
		true  ->
		    Pos = (UnicodeCP - First)*2,
		    <<_:Pos/binary, Index:?U16, ?SKIP>> = IndexArray,
		    Index
	    end;
	%% Format8: Mixed 16/32 and pure 32 bit mappings
	<<_:IndexMap/binary, 8:16, ?SKIP>> -> 0;
	%% Format10: Mixed 16/32 and pure 32 bit mappings
	<<_:IndexMap/binary, 10:16, ?SKIP>> -> 0;
	%% Format12/13: Mixed 16/32 and pure 32 bit mappings
	<<_:IndexMap/binary, Format:?U16, _:16, _:32, _:32, Count:?U32, Groups/binary>>
	  when Format =:= 12; Format =:= 13 ->
	    format_32_search(0, Count, Groups, UnicodeCP, Format);
	%% Unsupported ( not specified )
	_ -> 0
    end.

%%
%% Bake a bitmap/texture with codepoints from Chars
%% Returns {TextureDim, TextureBin, fun GetCharInfo/3}.
%% Invoke GetCharInfo(X,Y,Char) -> {{X0,Y0,X1,Y1},{S0,T0,S1,T1},XAdv}
%%     To get quad position, UV's and horizontal char advance
%% Coordinate system assumes Y increases upwards
%% Characters will extend both above and below the current position;
%% see discussion of "BASELINE" above.
%%
-spec bake_bitmap(Font::ttf(), FontH::integer(), Chars::[integer()]) ->
			 {Bitmap::size(), Bitmap::binary(), GetCharInfo::function()}.
bake_bitmap(#ttf_info{}=Font, FontHeightInPixels, Chars)
  when is_list(Chars), is_integer(FontHeightInPixels) ->
    Scale = scale_for_pixel_height(Font, FontHeightInPixels),
    GI = fun(Char, Glyph) ->
		 try
		     {Advance, _} = get_glyph_h_metrics(Font, Glyph),
		     {Dim, Off, Bin} = get_glyph_bitmap(Font, Scale, Glyph),
		     #gi{dim=Dim, off=Off, advance=Advance*Scale,
			 char=Char, glyph=Glyph, bin=Bin}
		 catch Exit:Reason ->
			 %%io:format("Error Parsing ~p~n",[Char]),
			 erlang:raise(Exit, Reason, erlang:get_stacktrace())
		 end
	 end,
    CharInfo = fun(Char, All) ->
		       Glyph = find_glyph_index(Font, Char),
		       case GI(Char, Glyph) of
			   skip -> All;
			   Info -> [Info|All]
		       end
	       end,
    %% Add unknown glyphinfo if not present
    GlyphInfos = case lists:keysort(#gi.glyph,lists:foldl(CharInfo, [], Chars)) of
		     WithDefault = [#gi{glyph=0}|_] -> WithDefault;
		     WithOut -> [GI(-1, 0)|WithOut]
		 end,
    {UniqueGlyphs,Duplicates} = remove_duplicates(GlyphInfos, undefined, undefined, [], []),
    HeightSort = fun(#gi{dim={_,H0}}, #gi{dim={_,H1}}) ->
			 H0 =< H1
		 end,
    bake_bitmap_impl(lists:sort(HeightSort, UniqueGlyphs), Duplicates).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

get_font_info(0, _, _, _, _, _, _) -> [];
get_font_info(N, <<PId:?U16, EId:?U16, LId:?U16, NId:?U16,
		   Length:?U16, StrOffset:?U16, Rest/binary>>, Strings,
	      WIds, WPlatform, WEnc, WLang) ->
    <<_:StrOffset/binary, String:Length/binary, ?SKIP>> = Strings,
    Platform = platform(PId),
    Encoding = encoding(EId, Platform),
    Lang = language(LId, Platform),
    Enc = check_enc(Encoding, WEnc),
    case lists:member(info(NId), WIds) of
	true when Platform =:= WPlatform, Enc, Lang =:= WLang ->
	    %% io:format("Encoding ~p Platform ~p Eid ~p~n",[Encoding, Platform, EId]),
	    [{info(NId), string(String, Encoding)}|
	     get_font_info(N-1, Rest, Strings, WIds, WPlatform, WEnc, WLang)];
	_ ->
	    get_font_info(N-1, Rest, Strings, WIds, WPlatform, WEnc, WLang)
    end.

check_enc(A, A) -> true;
check_enc({unicode,_}, unicode) -> true;
check_enc({unicode,_,_}, unicode) -> true;
check_enc(_, _) -> false.

%% PLATFORM ID
-define(PLATFORM_ID_UNICODE,  0).
-define(PLATFORM_ID_MAC,      1).
-define(PLATFORM_ID_ISO,      2).
-define(PLATFORM_ID_MICROSOFT,3).

%%  encodingID for PLATFORM_ID_UNICODE
-define(UNICODE_EID_UNICODE_1_0    ,0).
-define(UNICODE_EID_UNICODE_1_1    ,1).
-define(UNICODE_EID_ISO_10646      ,2).
-define(UNICODE_EID_UNICODE_2_0_BMP,3).
-define(UNICODE_EID_UNICODE_2_0_FULL,4).

%% encodingID for PLATFORM_ID_MICROSOFT
-define(MS_EID_SYMBOL        ,0).
-define(MS_EID_UNICODE_BMP   ,1).
-define(MS_EID_SHIFTJIS      ,2).
-define(MS_EID_UNICODE_FULL  ,10).

%% encodingID for PLATFORM_ID_MAC; same as Script Manager codes
-define(MAC_EID_ROMAN        ,0).
-define(MAC_EID_JAPANESE     ,1).
-define(MAC_EID_CHINESE_TRAD ,2).
-define(MAC_EID_KOREAN       ,3).
-define(MAC_EID_ARABIC       ,4).
-define(MAC_EID_HEBREW       ,5).
-define(MAC_EID_GREEK        ,6).
-define(MAC_EID_RUSSIAN      ,7).

platform(0) -> unicode;
platform(1) -> mac;
platform(2) -> iso;
platform(3) -> microsoft;
platform(Id) -> Id.

encoding(0, unicode) -> {unicode, {1,0}};
encoding(1, unicode) -> {unicode, {1,1}};
encoding(2, unicode) -> iso_10646;
encoding(3, unicode) -> {unicode, bmp, {2,0}};
encoding(4, unicode) -> {unicode, full,{2,0}};

encoding(0, microsoft)  -> symbol;
encoding(1, microsoft)  -> {unicode, bmp};
encoding(2, microsoft)  -> shiftjis;
encoding(10, microsoft) -> {unicode, bmp};

encoding(0, mac) ->  roman        ;
encoding(1, mac) ->  japanese     ;
encoding(2, mac) ->  chinese_trad ;
encoding(3, mac) ->  korean       ;
encoding(4, mac) ->  arabic       ;
encoding(5, mac) ->  hebrew       ;
encoding(6, mac) ->  greek        ;
encoding(7, mac) ->  russian      ;

encoding(Id, _) -> Id.

info(0) -> copyright;
info(1) -> family;
info(2) -> subfamily;
info(3) -> unique_subfamily;
info(4) -> fullname;
info(5) -> version;
info(6) -> postscript_name;
info(7) -> trademark_notice;
info(8) -> manufacturer_name;
info(9) -> designer;
info(10) -> description;
info(11) -> url_vendor;
info(12) -> url_designer;
info(13) -> license_descr;
info(14) -> url_license;
%info(15) -> reserved;
info(16) -> preferred_family;
info(17) -> preferred_subfamily;
%%info(18) -> compatible_full; %% Mac only
info(19) -> sample_text;
info(Id) -> Id.

string(String, roman) ->
    unicode:characters_to_list(String, latin1);
string(String, {unicode, _}) ->
    unicode:characters_to_list(String, utf16);
string(String, {unicode, bmp, _}) ->
    unicode:characters_to_list(String, utf16);
string(String, {unicode, full, _}) ->
    unicode:characters_to_list(String, utf32);
string(String, _) ->
    String.

%% languageID for PLATFORM_ID_MICROSOFT; same as LCID...
%% problematic because there are e.g. 16 english LCIDs and 16 arabic LCIDs
-define(MS_LANG_ENGLISH     ,16#0409).
-define(MS_LANG_CHINESE     ,16#0804).
-define(MS_LANG_DUTCH       ,16#0413).
-define(MS_LANG_FRENCH      ,16#040c).
-define(MS_LANG_GERMAN      ,16#0407).
-define(MS_LANG_HEBREW      ,16#040d).
-define(MS_LANG_ITALIAN     ,16#0410).
-define(MS_LANG_JAPANESE    ,16#0411).
-define(MS_LANG_KOREAN      ,16#0412).
-define(MS_LANG_RUSSIAN     ,16#0419).
-define(MS_LANG_SPANISH     ,16#0409).
-define(MS_LANG_SWEDISH     ,16#041D).

%% languageID for PLATFORM_ID_MAC
-define(MAC_LANG_ENGLISH      ,0 ).
-define(MAC_LANG_ARABIC       ,12).
-define(MAC_LANG_DUTCH        ,4 ).
-define(MAC_LANG_FRENCH       ,1 ).
-define(MAC_LANG_GERMAN       ,2 ).
-define(MAC_LANG_HEBREW       ,10).
-define(MAC_LANG_ITALIAN      ,3 ).
-define(MAC_LANG_JAPANESE     ,11).
-define(MAC_LANG_KOREAN       ,23).
-define(MAC_LANG_RUSSIAN      ,32).
-define(MAC_LANG_SPANISH      ,6 ).
-define(MAC_LANG_SWEDISH      ,5 ).
-define(MAC_LANG_CHINESE_SIMPLIFIED ,33).
-define(MAC_LANG_CHINESE_TRAD ,19).

language(16#0409, microsoft) -> english ;
language(16#0804, microsoft) -> chinese ;
language(16#0413, microsoft) -> dutch   ;
language(16#040c, microsoft) -> french  ;
language(16#0407, microsoft) -> german  ;
language(16#040d, microsoft) -> hebrew  ;
language(16#0410, microsoft) -> italian ;
language(16#0411, microsoft) -> japanese;
language(16#0412, microsoft) -> korean  ;
language(16#0419, microsoft) -> russian ;
%%language(16#0409, microsoft) -> spanish ;
language(16#041d, microsoft) -> swedish ;

language(0 , mac) -> english ;
language(12, mac) -> arabic  ;
language(4 , mac) -> dutch   ;
language(1 , mac) -> french  ;
language(2 , mac) -> german  ;
language(10, mac) -> hebrew  ;
language(3 , mac) -> italian ;
language(11, mac) -> japanese;
language(23, mac) -> korean  ;
language(32, mac) -> russian ;
language(6 , mac) -> spanish ;
language(5 , mac) -> swedish ;
language(33, mac) -> chinese_simplified ;
language(19, mac) -> chinese ;

language(Id, _) -> Id.

name(Id) -> Id.

init_font_1(Bin0, Opts) ->
    try
	Index = proplists:get_value(index, Opts, 0),
	Bin  = get_font_from_offset(Bin0, Index),
	is_font(Bin) orelse throw(bad_ttf_file),
	CMap = find_table(Bin, <<"cmap">>),
	Loca = find_table(Bin, <<"loca">>),
	Head = find_table(Bin, <<"head">>),
	Glyf = find_table(Bin, <<"glyf">>),
	Hhea = find_table(Bin, <<"hhea">>),
	Hmtx = find_table(Bin, <<"hmtx">>),
	Kern = find_table(Bin, <<"kern">>),
	case [W || W <- [CMap, Loca, Head, Glyf, Hhea, Hmtx], W =:= false] of
	    [false|_] -> throw(bad_ttf_file);
	    _ ->ok
	end,
	NumGlyphs = num_glyphs(Bin),
	IndexMap  = find_cmap(CMap, Bin),
	Skip = Head+50,
	<<_:Skip/binary, LocFormat:?U16, ?SKIP>> = Bin,
	{ok, #ttf_info{data = Bin,  num_glyphs = NumGlyphs,
		       loca = Loca, head = Head,
		       glyf = Glyf, hhea = Hhea,
		       hmtx = Hmtx, kern = Kern,
		       index_map = IndexMap,
		       index_to_loc_format = LocFormat
		      }}
    catch throw:Error ->
	    {error, Error};
	  _:_ ->
	    {error, internal_error}
    end.

num_glyphs(Bin) ->
    case find_table(Bin, <<"maxp">>) of
	false -> 16#ffff;
	Offset0 ->
	    Offset = Offset0+4,
	    <<_:Offset/binary, NG:?U16, ?SKIP>> = Bin,
	    NG
    end.

find_cmap(Cmap, Bin) ->
    <<_:Cmap/binary, _:16, NumTables:?U16, Data/binary>> = Bin,
    Cmap + find_cmap1(NumTables, Data).

find_cmap1(0, _) -> throw(unsupported_format);
find_cmap1(_, <<?PLATFORM_ID_MICROSOFT:?U16,
		?MS_EID_UNICODE_BMP:?U16, Offset:?U32, ?SKIP>>) -> Offset;
find_cmap1(_, <<?PLATFORM_ID_MICROSOFT:?U16,
		?MS_EID_UNICODE_FULL:?U16, Offset:?U32, ?SKIP>>) -> Offset;
find_cmap1(NumTables, <<_:64, Next/binary>>) ->
    find_cmap1(NumTables-1, Next).

find_table(<<_:32, NumTables:?U16, _SR:16, _ES:16, _RS:16, Tables/binary>>, Tag) ->
    find_table(NumTables, Tag, Tables).
find_table(0, _, _) -> false;
find_table(_, Tag, <<Tag:4/binary, _CheckSum:32, Offset:?U32, _Len:32, ?SKIP>>) ->
    Offset;
find_table(Num, Tag, <<_Tag:32, _CheckSum:32, _Offset:32, _Len:32, Next/binary>>) ->
    find_table(Num-1, Tag, Next).

get_font_from_offset(Bin, 0) -> Bin;
get_font_from_offset(Bin, Index) ->
    is_font(Bin) andalso exit(not_a_font_collection),
    Skip = Index * 14,
    case Bin of
	<<"ttcf", 0,V,0,0, N:32, _:Skip/binary, FontPos:32, ?SKIP >>
	  when V =:= 1, V=:= 2 ->
	    (Index < N) orelse exit(bad_font_index),
	    <<_:FontPos/binary, FontBin/binary>> = Bin,
	    FontBin;
	_ ->
	    exit(not_a_supported_font_collection)
    end.

is_font(<<1,0,0,0,?SKIP>>) -> true;  %% Truetype 1
is_font(<<"typ1",?SKIP>>)  -> true;  %% Truetype with type 1 font, not supported
is_font(<<"OTTO",?SKIP>>)  -> true;  %% OpenType with CFF
is_font(<<0,1,0,0,?SKIP>>) -> true;  %% OpenType with 1.0
is_font(_) -> false.

get_glyf_offset(#ttf_info{num_glyphs=NumGlyphs}, Glyph)
  when Glyph >= NumGlyphs ->
    -1; %% Out of range
get_glyf_offset(#ttf_info{index_to_loc_format=0, data=Bin, loca=Loca, glyf=Glyf}, Glyph) ->
    Skip = Glyph*2,
    <<_:Loca/binary, _:Skip/binary, G1:?U16, G2:?U16, ?SKIP>> = Bin,
    case G1 == G2 of
	true -> -1;
	false -> Glyf + G1 * 2
    end;
get_glyf_offset(#ttf_info{index_to_loc_format=1, data=Bin, loca=Loca, glyf=Glyf}, Glyph) ->
    Skip = Glyph*4,
    <<_:Loca/binary, _:Skip/binary, G1:?U32, G2:?U32, ?SKIP>> = Bin,
    case G1 == G2 of
	true -> -1; %% Length is zero
	false -> Glyf + G1
    end;
get_glyf_offset(_, _) -> %% unknown glyph map format
    -1.

get_glyph_shape_impl(_TTF, Offset)
  when Offset < 0 -> [];
get_glyph_shape_impl(TTF = #ttf_info{data=Bin}, Offset) ->
    <<_:Offset/binary, NumberOfContours:?S16,
      _XMin:16, _YMin:16, _XMax:16, _YMax:16,
      GlyphDesc/binary>> = Bin,
    if NumberOfContours > 0 ->
	    %% Single Glyph
	    Skip = NumberOfContours*2 - 2,
	    <<_:Skip/binary, Last:?U16, InsLen:?U16, Instr/binary>> = GlyphDesc,
	    N = 1 + Last,
	    <<_:InsLen/binary, FlagsBin/binary>> = Instr,
	    %%io:format("Conts ~p ~p ~p~n",[NumberOfContours, InsLen, N]),
	    {Flags, XCoordsBin} = parse_flags(N, 0, FlagsBin, []),
	    {XCs, YCoordsBin} = parse_coords(Flags, XCoordsBin, 0, 2, []),
	    {YCs, _} = parse_coords(Flags, YCoordsBin, 0, 4, []),
	    N = length(Flags),
	    setup_vertices(Flags, XCs, YCs, GlyphDesc);
       NumberOfContours =:= -1 ->
	    %% Several Glyphs (Compund shapes)
	    get_glyph_shapes(GlyphDesc, TTF, []);
       NumberOfContours < -1 ->
	    exit(bad_ttf);
       NumberOfContours =:= 0 ->
	    []
    end.

parse_flags(N, 0, <<Flag:8, Rest/binary>>, Flags)
  when N > 0 ->
    case (Flag band 8) > 1 of
	false ->
	    parse_flags(N-1, 0, Rest, [Flag|Flags]);
        true ->
	    <<Repeat:8, Next/binary>> = Rest,
	    parse_flags(N-1, Repeat, Next, [Flag|Flags])
    end;
parse_flags(N, R, Rest, Flags = [Prev|_])
  when N > 0 ->
    parse_flags(N-1, R-1, Rest, [Prev|Flags]);
parse_flags(0, 0, Rest, Flags) -> {lists:reverse(Flags), Rest}.

%% repeat(0, _, Flags) -> Flags;
%% repeat(N, Flag, Flags) -> repeat(N-1, Flag, [Flag|Flags]).

parse_coords([Flag|Flags], <<DX:8, Coords/binary>>, X0, Mask, Xs)
  when (Flag band Mask) > 1, (Flag band (Mask*8)) > 1 ->
    X = X0+DX,
    parse_coords(Flags, Coords, X, Mask, [X|Xs]);
parse_coords([Flag|Flags], <<DX:8, Coords/binary>>, X0, Mask, Xs)
  when (Flag band Mask) > 1 ->
    X = X0-DX,
    parse_coords(Flags, Coords, X, Mask, [X|Xs]);
parse_coords([Flag|Flags], Coords, X, Mask, Xs)
  when (Flag band (Mask*8)) > 1 ->
    parse_coords(Flags, Coords, X, Mask, [X|Xs]);
parse_coords([_|Flags], <<DX:?S16, Coords/binary>>, X0, Mask, Xs) ->
    X = X0 + DX,
    parse_coords(Flags, Coords, X, Mask, [X|Xs]);
parse_coords([], Rest, _, _, Xs) ->
    {lists:reverse(Xs), Rest}.

setup_vertices(Flags, XCs, YCs, GlyphDesc) ->
    setup_vertices(Flags, XCs, YCs, GlyphDesc, 0, -1, {0,0}, false,false, []).

setup_vertices([Flag|Fs0], [X|XCs0], [Y|YCs0], GD, StartC, Index,
	       S0, WasOff, StartOff0, Vs0)
  when StartC < 2 ->
    Vs1 = case StartC of
	      0 -> Vs0; %% First
	      1 -> close_shape(Vs0, S0, WasOff, StartOff0)
	  end,
    %% Start new one
    <<Next0:?U16, NextGD/binary>> = GD,
    Next = Next0-Index,
    case (Flag band 1) =:= 0 of
	true  ->
	    StartOff = {X,Y}, %% Save for warparound
	    [FN|Fs1]  = Fs0,
	    [XN|Xcs1] = XCs0,
	    [YN|Ycs1] = YCs0,
	    {S,Skip,Fs,XCs,YCs} =
		case ((FN band 1) =:= 0) of
		    true -> %% Next is also off
			{{(X+XN) div 2, (Y+YN) div 2},0,
			 Fs0, XCs0, YCs0};
		    false ->
			{{XN, YN},1,Fs1,Xcs1,Ycs1}
		end,
	    %%io:format("SOff ~p ~p ~p~n",[(Flag band 1) =:= 0, S, Next]),
	    Vs = set_vertex(Vs1, move, S, {0,0}),
	    setup_vertices(Fs,XCs,YCs,NextGD,Next-Skip,Next0,S,false,StartOff,Vs);
	false ->
	    S = {X,Y},
	    %%io:format("Start ~p ~p ~p~n",[(Flag band 1) =:= 0, S, Next]),
	    Vs = set_vertex(Vs1, move, S, {0,0}),
	    setup_vertices(Fs0,XCs0,YCs0,NextGD,Next,Next0,S,false,false,Vs)
    end;
setup_vertices([Flag|Fs], [X|XCs], [Y|YCs], GD, Next,Index,S,WasOff,StartOff,Vs0) ->
    %%io:format("~p ~p~n",[(Flag band 1) =:= 0, WasOff /= false]),
    case {(Flag band 1) =:= 0, WasOff} of
	{true, {Cx,Cy}} ->
	    %%  two off-curve control points in a row means interpolate an on-curve midpoint
	    Int = {(X+Cx) div 2, (Y+Cy) div 2},
	    Vs = set_vertex(Vs0, curve, Int, WasOff),
	    setup_vertices(Fs,XCs,YCs, GD, Next-1,Index,S,{X,Y}, StartOff, Vs);
	{true, false} ->
	    setup_vertices(Fs,XCs,YCs, GD, Next-1,Index,S,{X,Y}, StartOff, Vs0);
	{false,false} ->
	    Vs = set_vertex(Vs0, line, {X,Y}, {0,0}),
	    setup_vertices(Fs,XCs,YCs, GD, Next-1,Index,S,false, StartOff, Vs);
	{false,C} ->
	    Vs = set_vertex(Vs0, curve, {X,Y}, C),
	    setup_vertices(Fs,XCs,YCs, GD, Next-1,Index,S,false, StartOff, Vs)
    end;
setup_vertices([], [], [], _, _Next, _, S, WasOff, StartOff, Vs) ->
    lists:reverse(close_shape(Vs, S, WasOff, StartOff)).

close_shape(Vs0, S={SX,SY}, C={CX,CY}, SC={_SCX,_SCY}) ->
    Vs1 = set_vertex(Vs0, curve, {(SX+CX) div 2, (SY+CY) div 2}, C),
    set_vertex(Vs1, curve, S, SC);
close_shape(Vs, S, false, SC={_SCX,_SCY}) ->
    set_vertex(Vs, curve, S, SC);
close_shape(Vs, S, C={_CX,_CY}, false) ->
    set_vertex(Vs, curve, S, C);
close_shape(Vs, S, false, false) ->
    set_vertex(Vs, line, S, {0,0}).

set_vertex(Vs, Mode, Pos, C) ->
    %%io:format("V ~p ~p ~p~n",[Pos, C, Mode]),
    [#vertex{type=Mode, pos=Pos, c=C}|Vs].

get_glyph_shapes(<<Flags:?S16, GidX:?S16, GlyphDesc0/binary>>, Font, Vs0) ->
    {ScaleInfo,GlyphDesc} = find_trans_scales(Flags, GlyphDesc0),
    Vs1 = get_glyph_shape(Font, GidX),
    Vs = scale_vertices(Vs1, ScaleInfo, Vs0),
    case (Flags band (1 bsl 5)) > 1 of
	true -> %% More Compontents
	    get_glyph_shapes(GlyphDesc, Font, Vs);
	false ->
	    lists:reverse(Vs)
    end.

find_trans_scales(Flags,
		  <<Mtx4:?S16, Mtx5:?S16, GlyphDesc/binary>>)
  when (Flags band 3) > 2 ->
    find_trans_scales(Flags, Mtx4, Mtx5, GlyphDesc);
find_trans_scales(Flags, <<Mtx4:8, Mtx5:8, GlyphDesc/binary>>)
  when (Flags band 2) > 1 ->
    find_trans_scales(Flags, Mtx4, Mtx5, GlyphDesc).
%% @TODO handle matching point
%%find_trans_scales(Flags, GlyphDesc0) ->

find_trans_scales(Flags, Mtx4, Mtx5, <<Mtx0:?S16, GlyphDesc/binary>>)
  when (Flags band (1 bsl 3)) > 1 ->
    %% We have a scale
    S = 1 / 16384,
    {calc_trans_scales(Mtx0*S, 0, 0, Mtx0*S, Mtx4, Mtx5),GlyphDesc};
find_trans_scales(Flags, Mtx4, Mtx5, <<Mtx0:?S16, Mtx3:?S16, GlyphDesc/binary>>)
  when (Flags band (1 bsl 6)) > 1 ->
    %% We have a X and Y scale
    S = 1 / 16384,
    {calc_trans_scales(Mtx0*S, 0, 0, Mtx3*S, Mtx4, Mtx5), GlyphDesc};
find_trans_scales(Flags, Mtx4, Mtx5,
		  <<Mtx0:?S16, Mtx1:?S16,
		    Mtx2:?S16, Mtx3:?S16, GlyphDesc/binary>>)
  when (Flags band (1 bsl 7)) > 1 ->
    %% We have a two by two
    S = 1 / 16384,
    {calc_trans_scales(Mtx0*S, Mtx1*S, Mtx2*S, Mtx3*S, Mtx4, Mtx5), GlyphDesc};
find_trans_scales(_, Mtx4, Mtx5, GlyphDesc) ->
    {calc_trans_scales(1.0, 0.0, 0.0, 1.0, Mtx4, Mtx5), GlyphDesc}.

calc_trans_scales(Mtx0, Mtx1, Mtx2, Mtx3, Mtx4, Mtx5) ->
    {math:sqrt(square(Mtx0)+square(Mtx1)),
     math:sqrt(square(Mtx2)+square(Mtx3)), Mtx0, Mtx1, Mtx2, Mtx3, Mtx4, Mtx5}.

scale_vertices([#vertex{pos={X,Y}, c={CX,CY}, type=Type}|Vs],
	       SI={M,N, Mtx0, Mtx1, Mtx2, Mtx3, Mtx4, Mtx5}, Acc) ->
    V = #vertex{type=Type,
		pos = {round(M*(Mtx0*X+Mtx2*Y+Mtx4)),
		       round(N*(Mtx1*X+Mtx3*Y+Mtx5))},
		c   = {round(M*(Mtx0*CX+Mtx2*CY+Mtx4)),
		       round(N*(Mtx1*CX+Mtx3*CY+Mtx5))}},
    scale_vertices(Vs, SI, [V|Acc]);
scale_vertices([], _, Acc) -> Acc.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

format_4_index(_, Unicode) when Unicode >= 16#FFFF -> 0;
format_4_index(<<SegCountX2:?U16, SearchRange0:?U16, EntrySel:?U16,
		 RangeShift:?U16, Table/binary>>, Unicode) ->
    %% SegCount    = SegCountX2 div 2,
    SearchRange = SearchRange0 div 2,
    %% Binary Search
    <<EndCode:SegCountX2/binary, 0:16,
      StartCode:SegCountX2/binary,
      IdDelta:SegCountX2/binary,
      IdRangeOffset/binary  %% Also includes  GlyphIndexArray/binary
    >> = Table,
    %% they lie from endCount .. endCount + segCount
    %% but searchRange is the nearest power of two, so...
    Search = case EndCode of
		 <<_:RangeShift/binary, Search0:?U16, ?SKIP>>
		   when Unicode >= Search0 ->
		     RangeShift;
		 _ -> 0
	     end,
    Item = format_4_search(EntrySel, Search-2, SearchRange, EndCode, Unicode),
    case EndCode of
	<<_:Item/binary, Assert:16, ?SKIP>> ->
	    true = Unicode =< Assert;
	_ -> exit(assert)
    end,
    <<_:Item/binary, Start:?U16, ?SKIP>> = StartCode,
    %% <<_:Item/binary, End:?U16, ?SKIP>> = EndCode,
    <<_:Item/binary, Offset:?U16, ?SKIP>> = IdRangeOffset,

    if
	Unicode < Start ->
	    0;
	Offset =:= 0 ->
	    <<_:Item/binary, Index:?S16, ?SKIP>> = IdDelta,
	    Index + Unicode;
	true ->
	    Skip = Item + Offset + (Unicode - Start)*2,
	    <<_:Skip/binary, Index:?U16, ?SKIP>> = IdRangeOffset,
	    Index
    end.

format_4_search(EntrySel, Start, SearchRange, Bin, Unicode) when EntrySel > 0 ->
    Index = Start + SearchRange,
    case Bin of
	<<_:Index/binary, End:?U16, ?SKIP>> when Unicode > End ->
	    format_4_search(EntrySel-1, Start+SearchRange, SearchRange div 2, Bin, Unicode);
	_ ->
	    format_4_search(EntrySel-1, Start, SearchRange div 2, Bin, Unicode)
    end;
format_4_search(_, Search, _, _, _) ->
    Search+2.

format_32_search(Low, High, Groups, UnicodeCP, Format)
  when Low < High ->
    Mid = Low + ((High - Low) div 2),
    MidIndex = Mid*12,
    <<_:MidIndex/binary, Start:?U32, End:?U32, Glyph:?U32, ?SKIP>> = Groups,
    if
	UnicodeCP < Start ->
	    format_32_search(Low, Mid, Groups, UnicodeCP, Format);
	UnicodeCP > End ->
	    format_32_search(Mid+1, High, Groups, UnicodeCP, Format);
	Format =:= 12 ->
	    Glyph+UnicodeCP-Start;
	Format =:= 13 ->
	    Glyph
    end;
format_32_search(_, _, _, _, _) -> 0.

glyphs_kern_search(L, R, Needle, Info)
  when L =< R ->
    M = (L+R) div 2,
    Skip = M*6,
    <<_:Skip/binary, Straw:?U32, Res:?S16>> = Info,
    if Needle < Straw ->
	    glyphs_kern_search(L, M-1, Needle, Info);
       Needle > Straw ->
	    glyphs_kern_search(M+1, R, Needle, Info);
       true ->
	    Res
    end;
glyphs_kern_search(_L, _R, _Needle, _Info) -> 0.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

rasterize([], _, _, _, _, _, _, {W,H}) ->
    Fill = W*H,
    <<0:Fill/unit:8>>;
rasterize(Vertices, FlatnessInPixels,
	  Scale, Shift, XOFF, YOFF,
	  Invert, Dim = {_W,H}) ->
    {ScaleX,ScaleY} = scale(Scale),
    MinScale = min(ScaleY,ScaleX),
    Winding = flatten_curves(Vertices, FlatnessInPixels / MinScale),
    %% io:format("Windings ~p ~n",[length(Winding)]),
    %% print(Winding, 0),
    InvScaleY = if Invert -> -ScaleY; true -> ScaleY end,
    %% vsubsample should divide 255 evenly; otherwise we won't reach full opacity
    VSubSample = if H < 8 -> 15; true -> 5 end,
    Make = fun(ContWind, Edges) ->
		   make_edges(ContWind, Invert, VSubSample,
			      {ScaleX,InvScaleY}, shift(Shift), Edges)
	   end,
    Edges = lists:foldl(Make, [], Winding),
    Sorted = lists:sort(fun edge_sort/2, lists:reverse(Edges)),
    %%print(Sorted, 0),
    rasterize_ordered(Sorted, VSubSample, XOFF, YOFF, Dim).

make_edges([{_,Y}|Rest=[{_,Y}|_]], Invert, VSubSample, Scale, Shift, Eds) ->
    %% Skip if horizontal
    make_edges(Rest, Invert, VSubSample, Scale, Shift, Eds);
make_edges([{JX,JY}|Rest=[{KX,KY}|_]], Invert0, VSubSample,
	   Scale = {ScX, ScY}, Shift = {ShX,ShY}, Eds) ->
    Invert = if Invert0 -> JY > KY;
		true -> JY < KY
	     end,
    Edge = if Invert ->
		   {JX * ScX + ShX, JY * VSubSample * ScY + ShY,
		    KX * ScX + ShX, KY * VSubSample * ScY + ShY,
		    true};
	      true ->
		   {KX * ScX + ShX, KY * VSubSample * ScY + ShY,
		    JX * ScX + ShX, JY * VSubSample * ScY + ShY,
		    false}
	   end,
    make_edges(Rest, Invert0, VSubSample, Scale, Shift, [Edge|Eds]);
make_edges(_, _, _, _, _, Eds) -> Eds.

edge_sort({_,AY,_,_,_},{_,BY,_,_,_}) ->
    AY =< BY.

rasterize_ordered(Edges, VSubSample, XOFF, YOFF, {W, H}) ->
    MaxWeight = 255 div VSubSample, %% Weight per vertical scanline
    Active = [],
    Y = YOFF * VSubSample,
    %%io:format("No edges ~p~n",[length(Edges)]),
    rast_scanlines(H, VSubSample, Active, Edges, MaxWeight, XOFF, Y, W, []).

rast_scanlines(H, VSubSample, Active0, Edges, MW, XOFF, Y0, W, Rows)
  when H > 0 ->
    {Row,Y, Active, Left} = rast_scanline(0, VSubSample, Active0, Edges, MW,
					  XOFF, Y0, W, scanline(W)),
    rast_scanlines(H-1, VSubSample, Active, Left, MW,
		   XOFF, Y, W, add_scanline(Row,Rows));
rast_scanlines(_H, _VSubSample, _Active, _Edges, _, _XOFF, _Y, _W, Rows) ->
    list_to_binary(lists:reverse(Rows)).

rast_scanline(S, VSubSample, Active0, Edges, MaxWeight, XOFF, Y, W, ScanLine0)
  when S < VSubSample ->
    ScanY = Y + 0.5,
    Active1 = update_active(Active0, ScanY),
    Active2 = lists:sort(fun active_sort/2, Active1),
    {Active, RestEdges} = add_edges(Edges, ScanY, XOFF, Active2),
    %% Active =:= [] orelse io:format("~p:~.3f~n",[S, ScanY]),
    %% print(Active, 0),
    ScanLine = fill_active(Active, 0, 0, W, MaxWeight, ScanLine0),
    rast_scanline(S+1, VSubSample, Active, RestEdges, MaxWeight, XOFF, Y+1, W,
		  ScanLine);
rast_scanline(_S, _VSubSample, Active, Edges, _MaxWeight, _XOFF, Y, _W,
	      ScanLine) ->
    {ScanLine,Y, Active, Edges}.

scanline(W) ->
    array:new([{default,0}, {size,W}]).

add_scanline(ScanLine, Rows) ->
    Row = array:foldl(fun(_,Byte,Acc) ->
			      <<Acc/binary, (byte(Byte)):8>>
		      end, <<>> ,ScanLine),
    [Row|Rows].

-record(active, {x,dx,ey,valid}).
-define(FIXSHIFT, 10).
-define(FIX, (1 bsl ?FIXSHIFT)).
-define(FIXMASK, (?FIX-1)).

active_sort(#active{x=AX},#active{x=BX}) ->
    AX =< BX.

fill_active([#active{x=X, valid=W}|As], _X0, 0, Width, MaxWeight, ScanLine) ->
    fill_active(As, X, W, Width, MaxWeight, ScanLine);
fill_active([#active{x=X1, valid=W1}|As], X0, W0, Width, MaxWeight, ScanLine) ->
    W = W0+W1,
    I = X0 bsr ?FIXSHIFT,
    J = X1 bsr ?FIXSHIFT,
    if W /= 0; I >= Width; J < 0 ->
	    fill_active(As, X0, W, Width, MaxWeight, ScanLine);
       I =:= J -> %% X0,X1 are the same pixel, so compute combined coverage
	    SI = array:get(I, ScanLine) + byte(((X1-X0) * MaxWeight)
						   bsr ?FIXSHIFT),
	    fill_active(As, X0, W, Width, MaxWeight,
			array:set(I, byte(SI), ScanLine));
       true ->
	    {SC0,Start} =
		if I >= 0 -> %% Add AntiAliasing for X0
			SI0 = array:get(I, ScanLine),
			SI = SI0 + byte(((?FIX - (X0 band ?FIXMASK)) * MaxWeight)
					    bsr ?FIXSHIFT),
			{array:set(I, byte(SI), ScanLine), I+1};
		   true -> %% Clip
			{ScanLine, I}
		end,
	    {SC1,End} =
		if J < Width -> %% Add AntiAliasing for X1
			SJ0 = array:get(J, SC0),
			SJ = SJ0 + byte(((X1 band ?FIXMASK) * MaxWeight)
					    bsr ?FIXSHIFT),
			{array:set(J, byte(SJ), SC0), J};
		   true -> %% Clip
			{SC0, Width}
		end,
	    SC = fill_pixels(Start, End, MaxWeight, SC1),
	    fill_active(As, X0, W, Width, MaxWeight, SC)
    end;
fill_active([], _, _, _, _, ScanLine) ->
    ScanLine.

fill_pixels(I, J, MaxWeight, ScanLine) when I < J ->
    SI = array:get(I, ScanLine),
    SC = array:set(I, byte(SI + MaxWeight), ScanLine),
    fill_pixels(I+1, J, MaxWeight, SC);
fill_pixels(_, _, _, ScanLine) ->
    ScanLine.

%% update all active edges;
%% remove all active edges that terminate before the center of this scanline
update_active([#active{ey=Ey}|As], ScanY)
  when Ey =< ScanY ->
    update_active(As, ScanY);
update_active([A = #active{x=X,dx=Dx}|As], ScanY) ->
    [A#active{x=X+Dx}|update_active(As, ScanY)];
update_active([], _) -> [].

add_edges([Edge={_,Y0,_,Y1,_}|Eds], ScanY, XOFF, Active0)
  when Y0 =< ScanY, Y1 > ScanY ->
    Active = add_edge(Active0, make_active(Edge, XOFF, ScanY)),
    add_edges(Eds, ScanY, XOFF, Active);
add_edges([{_,Y0,_,_Y1,_}|Eds], ScanY, XOFF, Active)
  when Y0 =< ScanY ->
    add_edges(Eds, ScanY, XOFF, Active);
add_edges(Eds, _ScanY, _, Active) ->
    {Active, Eds}.

add_edge(As = [#active{x=AX}|_], Z=#active{x=ZX})
  when ZX =< AX -> [Z|As];
add_edge([First|As], New) ->
    [First|add_edge(As,New)];
add_edge([], Active) -> [Active].

make_active({X0,Y0,X1,Y1,Inv}, OffX, Start) ->
    true = Y0 =< Start, %% Assert
    DxDy = (X1-X0) / (Y1-Y0),
    %% round dx down to avoid going too far
    Dx = if DxDy < 0 -> -floor(-?FIX*DxDy);
	    true     ->  floor(?FIX*DxDy)
	 end,
    X = floor(?FIX * (X0 + DxDy * (Start - Y0))),
    Valid = if Inv -> 1; true -> -1 end,
    %% io:format("~p ~p ~.3f ~p ~n", [X-OffX*?FIX,Dx,Y1,Valid]),
    #active{x=X-OffX*?FIX, ey=Y1, dx=Dx, valid = Valid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

flatten_curves(Vs, Flatness0) ->
    Flatness = Flatness0*Flatness0,
    %% NumContours = length([V|| #vertex{type=move} <- Vs]),
    lists:reverse(flatten_curves(Vs, {0.0,0.0}, Flatness, [], [])).

flatten_curves([#vertex{type=move,pos=Point}|Vs], _, F, Cont, All) ->
    flatten_curves(Vs, Point, F, [Point], add_contour(Cont, All));
flatten_curves([#vertex{type=line,pos=Point}|Vs], _, F, Cont, All) ->
    flatten_curves(Vs, Point, F, [Point|Cont], All);
flatten_curves([#vertex{type=curve,pos=VP={PX,PY},c={CX,CY}}|Vs],
	       {X,Y}, F, Cont0, All) ->
    Cont = tesselate(X,Y, CX,CY, PX,PY, F, 0, Cont0),
    flatten_curves(Vs, VP, F, Cont, All);
flatten_curves([], _, _, Cont, All) ->
    add_contour(Cont, All).

add_contour([], All) -> All;
add_contour(Cont, All) ->
    [lists:reverse(Cont)|All].

tesselate(X0,Y0, X1,Y1, X2,Y2, F, Level, Cont0)
  when Level < 17 ->
    Mx = (X0 + 2*X1 + X2)/4,
    My = (Y0 + 2*Y1 + Y2)/4,
    %% Versus Directly Drawn Line
    Dx = (X0+X2)/2 - Mx,
    Dy = (Y0+Y2)/2 - My,
    if (Dx*Dx+Dy*Dy) > F ->
	    %% half-pixel error allowed... need to be smaller if AA
	    Cont1 = tesselate(X0,Y0, (X0+X1)/2.0,(Y0+Y1)/2.0, Mx,My, F, Level+1, Cont0),
	    tesselate(Mx,My, (X1+X2)/2.0,(Y1+Y2)/2.0, X2,Y2, F, Level+1, Cont1);
       true ->
	    [{X2,Y2}|Cont0]
    end;
tesselate(_X0,_Y0, _X1,_Y1, _X2,_Y2, _F, _Level, Cont) ->
    %% 65536 segments on one curve better be enough!
    Cont.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

remove_duplicates([GI=#gi{glyph=Glyph, char=From}|Rest], Glyph, To, GIs, Dups) ->
    remove_duplicates(Rest, Glyph, To, GIs, [GI#gi{char={From,To}}|Dups]);
remove_duplicates([GI=#gi{glyph=Glyph, char=Char}|Rest], _, _, GIs, Dups) ->
    remove_duplicates(Rest, Glyph, Char, [GI|GIs], Dups);
remove_duplicates([], _, _, GIs, Dups) ->
    {GIs, Dups}.

bake_bitmap_impl(OrderedGlyphs, Duplicates) ->
    {GlyphsInRows, BMDim={W,H}} = find_and_split_dim(OrderedGlyphs),
    Bin = bake_bitmap_impl(GlyphsInRows, 0, W, H, <<>>),
    %% io:format("~p ~p => ~p~n",[BMDim, byte_size(Bin), W*H]),
    true = W*H == byte_size(Bin),
    GIs = lists:foldl(fun({Infos, _}, Acc) -> Infos ++ Acc end, [], GlyphsInRows),
    %% Find default (unsupported glyph)
    #gi{pos={Dx,Dy},dim={Dw,Dh}, off={Dox,Doy}, advance=DAdv} =
	lists:keyfind(0, #gi.glyph, GIs),
    CIs = [{Char, #bci{x0=X0,y0=Y0,x1=X0+CW,y1=Y0+CH,ox=Ox,oy=-(Oy+CH),advance=Adv}} ||
	      #gi{char=Char, pos={X0,Y0},dim={CW,CH},off={Ox,Oy},advance=Adv} <- GIs,
	      Char >= 0],
    Default = #bci{x0=Dx,y0=Dy,x1=Dx+Dw,y1=Dy+Dh,ox=Dox,oy=-(Doy+Dh), advance=DAdv},
    BGI0 = array:from_orddict(lists:sort(CIs), Default),
    BGI = lists:foldl(fun(#gi{glyph=0}, Acc) -> Acc; %% already default
			 (#gi{char={From,To}}, Acc) ->
			      Info = array:get(To, Acc),
			      array:set(From, Info, Acc)
		      end, BGI0, Duplicates),
    InvW = 1/W, InvH = 1/H,
    CharInfo = fun(X,Y,Char) ->
		       #bci{x0=X0,y0=Y0,x1=X1,y1=Y1,ox=Ox,oy=Oy,advance=Adv}
			   = array:get(Char, BGI),
		       Rx = floor(X+Ox+0.5),
		       Ry = floor(Y+Oy+0.5),
		       Pos = {Rx, Ry, Rx+(X1-X0), Ry+(Y1-Y0)},
		       %io:format("~c ~p ~p ~p~n",[Char,Oy0, Y1-Y0, Pos]),
		       {Pos,
			{X0*InvW,Y1*InvH,X1*InvW,Y0*InvH},
			Adv}
	       end,
    {BMDim, Bin, CharInfo}.

bake_bitmap_impl([{Row,H}|Rows], HD, W, MaxH, Bin) ->
    bake_bitmap_impl(Rows, HD+H, W, MaxH, bake_bitmap_row(Row, 0, 0, W, H, Row, Bin));
bake_bitmap_impl([], H, W, MaxH, Bin) ->
    Fill = (MaxH - H) * W,
    <<Bin/binary, 0:Fill/unit:8>>.

bake_bitmap_row([#gi{dim={W,H},bin=Bitmap}|Row], WT, I, MaxW, MaxH, All, Bin)
  when I < MaxH, I < H ->
    Skip = W*I,
    <<_:Skip/binary, Pixels:W/binary, ?SKIP>> = Bitmap,
    bake_bitmap_row(Row, WT+W+1, I, MaxW, MaxH, All, <<Bin/binary, Pixels/binary, 0:8>>);
bake_bitmap_row([#gi{dim={W,H}}|Row], WT, I, MaxW, MaxH, All, Bin)
  when I < MaxH, I >= H ->
    %% Char is not high enough fill height with zero
    bake_bitmap_row(Row, WT+W+1, I, MaxW, MaxH, All, <<Bin/binary, 0:W/unit:8, 0:8>>);
bake_bitmap_row([], W, I, MaxW, MaxH, All, Bin)
  when I < MaxH ->
    Fill = (MaxW - W),
    bake_bitmap_row(All, 0, I+1, MaxW, MaxH, All, <<Bin/binary, 0:Fill/unit:8>>);
bake_bitmap_row(_, 0, MaxH, _MaxW, MaxH, _, Bin) ->
    %% ((Debug = (size(Bin) rem MaxW)) =:= 0) orelse
    %% 	io:format("Max {~p,~p} ~p ~p ~n",[_MaxW, MaxH, size(Bin), Debug]),
    Bin.

%%%%%%%%%%%%%%%%%%%%%%
%% Find how to best fit glyphs in powerOf2 (max 4096) width bitmap
find_and_split_dim(OrderedGlyphInfo) ->
    Ws = [8,16,32,64,128,256,512,1024,2048,4096],
    find_and_split_dim(Ws, OrderedGlyphInfo, {infinity, false, infinity, []}).

find_and_split_dim([Width|Ws], Ordered, Best = {Prev, false, D0, S0}) ->
    case split_glyphs(0, 0, 0, Width, Ordered, [], 0, []) of
	to_small ->
	    find_and_split_dim(Ws, Ordered, Best);
	Split = {Error, _, _D1, _} when Error =< Prev ->
	    %%io:format("  Yes ~p ~p~n",[Error, _D1]),
	    find_and_split_dim(Ws, Ordered, Split);
	{_Error, false, _D1, _} ->
	    %%io:format("  NO ~p ~p~n",[_Error, _D1]),
	    find_and_split_dim(Ws, Ordered, Best);
	%% Width is big enough to fit all in one row,
	%% skip testing larger widths
	{_, true, _, _} ->
	    {S0, D0}
    end;
find_and_split_dim(_, _, {_Error, _, Dim, Split}) ->
    %% io:format("Best ~p~n",[Dim]),
    {Split, Dim}.

split_glyphs(_, _, _, MaxW,[#gi{dim={GW,_GH}}|_], _, _, _)
  when MaxW =< GW ->
    to_small;
split_glyphs(W0, H, TotH, MaxW,Gis0=[GI=#gi{dim={GW,GH}}|Gis], Row0, Error, All) ->
    case W0 + GW +1 of
	W when W =< MaxW ->
	    split_glyphs(W, max(H, GH+1), TotH, MaxW, Gis,
			 [GI#gi{pos={W0,TotH}}|Row0],
			 Error, All);
	_W ->
	    WError = (MaxW - W0) * H,
	    {HError, Row} = calc_error(Row0, H, 0, []),
	    split_glyphs(0, 0, TotH+H, MaxW, Gis0, [], Error+WError+HError, [{Row,H}|All])
    end;
split_glyphs(W, H, TotH0, MaxW, [], Row0, Error0, All) ->
    WError  = (MaxW - W) * H,
    {HError, Row} = calc_error(Row0, H, 0, []),
    TotH1 = TotH0 + H,
    TotH = power2(TotH1),
    FillRowError = (TotH - TotH1) * MaxW,
    Error = Error0 + WError + HError + FillRowError,
    {Error, TotH0 == 0, {MaxW, TotH}, lists:reverse([{Row,H}|All])}.

calc_error([GI=#gi{dim={GW,GH}}|Gis], H, Error, Row) ->
    calc_error(Gis, H, Error + (H-GH)*GW, [GI|Row]);
calc_error([], _, Error, Row) ->
    {Error, Row}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

power2(X) ->
    XD = trunc(math:log(X) / math:log(2)),
    case math:pow(2, XD) of
	Y when X==Y -> X;
	Y when X =< Y -> trunc(Y);
	_ -> trunc(math:pow(2,XD+1))
    end.

floor(X) when X >= 0 -> trunc(X);
floor(X) ->
    Int = trunc(X),
    if (X - Int) < 0 -> Int-1;
       true -> Int
    end.
ceil(X) when X < 0 -> trunc(X);
ceil(X) ->
    Int = trunc(X),
    if (X - Int) > 0 -> Int+1;
       true -> Int
    end.

square(X) -> X*X.

scale(XY={_,_}) -> XY;
scale(X) -> {X,X}.

shift(XY={_,_}) -> XY;
shift(X) -> {X,X}.

%% byte(X) when X > 255 -> 255;
byte(X) -> X band 255.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% TEST/ DEBUG
%%

print([Ps|Ws], N) when is_list(Ps) ->
    New = print(Ps, N),
    print(Ws, New);
print([{X,Y}|Ps], N) ->
    io:format("~p ~p ~p~n", [N, X,Y]),
    print(Ps, N+1);
print([#active{x=X,dx=Dx, ey=Ey, valid=V}|Ps], N) ->
    io:format("Q ~p ~p ~p ~.3f ~p ~n", [N,X,Dx,float(Ey),V]),
    print(Ps, N+1);
print([{X0,Y0,X1,Y1,Inv}|Ps], N) ->
    io:format(" ~.3f ~.3f ~.3f ~.3f ~p ~n", [X0,Y0,X1,Y1,Inv]),
    print(Ps, N+1);

print([], N) ->N.

test_bitmap_gen(Font) ->
    Scale = scale_for_pixel_height(Font, 20),
    P = fun(Char) ->
		io:format("Char: ~c Ascii: ~p:~n", [Char,Char]),
		{{W,_H}, {_X0,_Y0}, Bin} = get_codepoint_bitmap(Font, Scale, Char),
		printbitmap(W, Bin, array:from_list(" .:ioVM@"), W)
	end,
    [P(Char) ||
	Char <-
	    %%lists:seq($a, $z) ++ lists:seq($A, $Z)
	    [$o]
    ].

printbitmap(I, <<Pixel:8, Bin/binary>>, A, W) when I > 0 ->
    io:format("~c", [array:get(Pixel bsr 5, A)]),
    printbitmap(I-1, Bin, A, W);
printbitmap(_, <<>>, _A, _W) ->
    io:format("_~n",[]),
    ok;
printbitmap(_, Bin, A, W) ->
    io:format("_~n",[]),
    printbitmap(W, Bin, A, W).

draw_bitmap({W,H}, Bin0) ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "Draw", [{size, {max(640,W+100), max(480, H+100)}}]),
    Panel = wxPanel:new(Frame, [{size, {min(640,W+100), max(480, H+100)}}]),
    Bin = << <<G:8, G:8, G:8 >> || <<G:8>> <= Bin0>>,
    Image = wxImage:new(W,H,Bin),
    Bmp = wxBitmap:new(Image),
    Paint = fun(_,_) ->
		    DC = wxPaintDC:new(Panel),
		    wxDC:drawBitmap(DC,Bmp, {10,30}),
		    wxPaintDC:destroy(DC)
	    end,
    wxFrame:connect(Panel, paint, [{callback, Paint}]),
    wxFrame:show(Frame),
    ok.

-include_lib("wx/include/gl.hrl").
-include_lib("wx/include/wx.hrl").

gl_init_font({{W,H}, Bin, CharI}) ->
    [Ftex] = gl:genTextures(1),
    gl:bindTexture(?GL_TEXTURE_2D, Ftex),
    gl:texImage2D(?GL_TEXTURE_2D, 0, ?GL_ALPHA, W,H, 0, ?GL_ALPHA, ?GL_UNSIGNED_BYTE, Bin),
    gl:texParameteri(?GL_TEXTURE_2D, ?GL_TEXTURE_MIN_FILTER, ?GL_LINEAR),
    {Ftex, CharI}.

gl_print_str(CharInfo, XS,YS, String) when is_function(CharInfo) ->
    lists:foldl(fun(Codepoint, X) ->
			_I={{X0,Y0,X1,Y1},{S0,T0,S1,T1},XAdv} = CharInfo(X, YS, Codepoint),
			Scale = 0,
			gl:texCoord2f(S0,T0), gl:vertex2f(X0,Y0),
			gl:texCoord2f(S1,T0), gl:vertex2f(X1+Scale,Y0),
			gl:texCoord2f(S1,T1), gl:vertex2f(X1+Scale,Y1+Scale),
			gl:texCoord2f(S0,T1), gl:vertex2f(X0,Y1+Scale),
			X + XAdv+Scale
		end, XS, String).


start_gl_window() ->
    Wx = wx:new(),
    Frame = wxFrame:new(Wx, -1, "GL Strings", [{size, {640, 480}}]),
    GLAttrib = [{attribList, [?WX_GL_RGBA, ?WX_GL_DOUBLEBUFFER,?WX_GL_DEPTH_SIZE, 24, 0]}],
    GL = wxGLCanvas:new(Frame, GLAttrib),
    wxWindow:connect(Frame, show),
    wxWindow:show(Frame),
    receive #wx{} -> ok end,
    wxGLCanvas:setCurrent(GL),
    {W,H} = wxWindow:getSize(GL),
    gl:matrixMode(?GL_PROJECTION),
    gl:pushMatrix(),
    gl:loadIdentity(),
    glu:ortho2D(0, W, 0, H),
    gl:matrixMode(?GL_MODELVIEW),
    gl:pushMatrix(),
    gl:loadIdentity(),
    gl:blendFunc(?GL_SRC_ALPHA, ?GL_ONE_MINUS_SRC_ALPHA),
    gl:enable(?GL_BLEND),
    gl:enable(?GL_TEXTURE_2D),
    gl:clearColor(1.0,1.0,1.0,1.0),
    gl:clear(?GL_COLOR_BUFFER_BIT bor ?GL_DEPTH_BUFFER_BIT),
    GL.

test_gl(Font) ->
    GL = start_gl_window(),
    {_Tex, CharInfo} = gl_init_font(Font),
    Redraw = fun(_,_) ->
		     DC = wxPaintDC:new(GL),
		     wxPaintDC:destroy(DC),
		     wxGLCanvas:setCurrent(GL),
		     redraw(GL, CharInfo)
	     end,
    wxWindow:connect(GL, paint, [{callback, Redraw}]),
    redraw(GL, CharInfo).

redraw(GL, CharInfo) ->
    gl:'begin'(?GL_QUADS),
    gl:color3ub(0,0,0),
    gl_print_str(CharInfo, 50, 100, "Hejlg Wroom Gjordes m" ++ [246] ++
		     "gligt Ibland 1234567890%$ " ++
		     [1513,1500,1493,1501,32,26085,26412,35486]),
    gl:'end'(),
    wxGLCanvas:swapBuffers(GL),
    ok.

test() ->
    try
	io:format("~n"),
	Dir = "/usr/share/fonts/truetype/msttcorefonts/",
	%% Dir = "c:/Windows/Fonts/",
	Files = filelib:wildcard("*.ttf", Dir),
	Init = fun(File0, Fonts) ->
		       File = filename:join(Dir, File0),
		       io:format("Reading ~s:  ",[File0]),
		       case truetype:init_font(File) of
			   {ok, Font} ->
			       Info = truetype:font_info(Font),
			       [io:format("~p: ~ts~n", [Id,Str]) || {Id, Str} <- Info],
			       [{proplists:get_value(family, Info), Font}|Fonts];
			   {error, Reason} ->
			       io:format("Init Error ~p~n", [Reason]),
			       Fonts
		       end
	       end,
	Fonts = lists:reverse(lists:foldl(Init,[],Files)),
	{value, {_,Font}} = lists:keysearch("Times New Roman", 1, Fonts),
	test_bitmap_gen(Font),
	FI = {Size, Bin, _Info} = truetype:bake_bitmap(Font,15,
						       %%[26085,26412,35486],
						       lists:seq(26000, 26500) ++
						       lists:seq(32,1624)
						      ),
	draw_bitmap(Size, Bin),
	test_gl(FI),
	ok
    catch _:Error ->
	    io:format("Error: ~140P in~n ~P~n", [Error, 10, erlang:get_stacktrace(), 20]),
	    {error, test, broke}
    end.
