-module(mandel).
-compile(export_all).
-define(THEME, neo).

%
%   concurrent mandel
%

init() ->
    code:add_path("../helper"),
    code:add_path("../introduction").

mandelbrot(Width, Height, X, Y, K, Depth, Cores) ->
    Trans = fun(W, H) ->
        complex:new(X + K * (W - 1), Y - K * (H - 1))
    end,

    MainProcess = self(),
    Interval = Height div Cores,
    trigger_rows(Width, Height, Interval, Trans, Depth, MainProcess),
    receive_rows(Height, Interval, []).

%
%   rows
%

trigger_rows(Width, Height, Interval, Trans, Depth, MainProcess) ->
    spawn_link(fun() -> rows(Width, Height, Height, Height - Interval, Trans, Depth, MainProcess, []) end),

    if
        Height > Interval -> trigger_rows(Width, Height - Interval, Interval, Trans, Depth, MainProcess);
        true -> ok
    end.

receive_rows(0, _, Result) -> Result;
receive_rows(Height, Interval, SubResult) ->
    receive
        {return, Height, Rows} ->
            receive_rows(Height - Interval, Interval, lists:append(Rows, SubResult));
        quit ->
            ok
    end.

rows(_, OrigHeight, Offset, Offset, _, _, Pid, Rows) -> Pid ! {return, OrigHeight, Rows};
rows(Width, OrigHeight, Height, Offset, Trans, Depth, Pid, Rows) ->
    New = row(Width, Height, Trans, Depth, []),
    rows(Width, OrigHeight, Height - 1, Offset, Trans, Depth, Pid, [New | Rows]).

row(0, _, _, _, Cols) -> Cols;
row(Width, Height, Trans, Depth, Cols) ->
    C = Trans(Width, Height),
    Color = theme:convert(brot:mandelbrot(C, Depth), Depth, ?THEME),
    row(Width - 1, Height, Trans, Depth, [Color | Cols]).

%
%   demo
%

brot(Width, Height, X, Y, X1, Depth, FileName) ->
    Cores = erlang:system_info(schedulers_online),
    brot(Width, Height, X, Y, X1, Depth, FileName, Cores).

brot(Width, Height, X, Y, X1, Depth, FileName, Cores) ->
    init(),

    K = (X1 - X) / Width,
    T0 = erlang:timestamp(),
    Image = mandelbrot(Width, Height, X, Y, K, Depth, Cores),
    T = timer:now_diff(erlang:timestamp(), T0),
    color:out("Config:", cyan),
    color:out(io_lib:format("Calculating using ~w processes", [Cores]), yellow),
    color:out(io_lib:format("Z = {~w, ~w}", [X, Y]), cyan),
    color:out(io_lib:format("x1 = ~w", [X1]), cyan),
    color:out(io_lib:format("Delta = ~w", [K]), cyan),
    color:out(io_lib:format("Picture generated in ~w ms", [T div 1000]), magenta),
    color:out(io_lib:format("Written to ~s", [FileName]), green),
    Path = "build/" ++ FileName,
    ppm:write(Path, Image).

% whole mandelbrot set
whole() -> brot(960, 560, -2.6, 1.2, 1.6, 64, "mandelbrot.ppm").

% pre given demo
demo() -> brot(1920, 1080, -0.14, 0.85, -0.13, 128, "demo.ppm").

% more demos
galaxy() -> 
    brot(1920, 1080, -0.005624999999999769, 0.766875, 0.013, 256, "galaxy.ppm").

% fjord image
fjord() -> s(-0.02,0.8,0).
fjord2() -> s(-0.01,0.8,0).

% spiral image
spiral() -> sdeep(0.294, 0.4835, 0.298).
spiral4kzoom() -> brot(3840, 2160, 0.2963, 0.4831, 0.29675, 256, "spiral4kzoom.ppm").

% search in full hd
s(X, Y, X1) -> brot(1920, 1080, X, Y, X1, 64, "s.ppm").
sdeep(X, Y, X1) -> brot(1920, 1080, X, Y, X1, 128, "sdeep.ppm").
s4k(X, Y, X1) -> brot(3840, 2160, X, Y, X1, 64, "s4k.ppm").
s8k(X, Y, X1) -> brot(7680, 4320, X, Y, X1, 64, "s8k.ppm").