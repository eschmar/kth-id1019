-module(theme).
-compile(export_all).

-define(GreenIsh, {34, 102, 102}).
-define(DarkGreenIsh, {0, 51, 51}).
-define(DarkDarkGreenIsh, {0, 49, 49}).
-define(Orange, {255, 172, 5}).
-define(DarkOrange, {89, 60, 0}).
-define(BrightOrange, {255, 118, 5}).
-define(Red, {255, 5, 5}).

%
%   theme - iteration to color converter
%

convert(Depth, Max) ->
    convert(Depth, Max, default).

convert(Depth, Max, default) ->
    Fraction = (Depth / Max) * 4,
    Section = trunc((Fraction)) rem 5,
    Offset = trunc(255 * (Fraction - Section)),
    case Section of
        0 -> {Offset, 0, 0};
        1 -> {255, Offset, 0};
        2 -> {255 - Offset, 255, 0};
        3 -> {0, 255, Offset};
        4 -> {0, 255 - Offset, 255}
    end;

convert(Depth, Max, inverted) ->
    Fraction = (Depth / Max) * 4,
    Section = trunc((Fraction)) rem 5,
    Offset = trunc(255 * (Fraction - Section)),
    case Section of
        0 -> {0, 0, Offset};
        1 -> {0, Offset, 255};
        2 -> {0, 255, 255 - Offset};
        3 -> {Offset, 255, 0};
        4 -> {255, 255 - Offset, 0}
    end;

convert(Depth, Max, neo) ->
    Fraction = (Depth / Max) * 4,
    Section = trunc(Fraction),
    Percent = (Fraction - Section),

    case Section of
        % 0 -> {0, 0, trunc(255 * (Fraction - Section))};
        0 -> gradient({1, 29, 51}, {6, 6, 29}, Percent); % green-ish
        1 -> gradient(?GreenIsh, ?DarkGreenIsh, Percent); % blue-ish
        2 -> gradient(?DarkDarkGreenIsh, ?Orange, Percent); % green-ish
        3 -> gradient(?Orange, ?DarkOrange, Percent); % yellow-ish
        4 -> gradient(?BrightOrange, ?Red, Percent)
    end.


gradient({R, G, B}, {R2, G2, B2}, Percent) ->
    {shade(R, R2, Percent), shade(G, G2, Percent), shade(B, B2, Percent)}.

shade(X, Y, Percent) -> trunc(X + (Percent * (Y - X))).
