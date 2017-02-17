-module(theme).
-compile(export_all).

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
        0 -> {0, 0, trunc(255 * (Fraction - Section))};
        1 -> gradient({34,102,102}, {0, 51, 51}, Percent); % blue-ish
        % 2 -> gradient({123,159, 53}, {53, 79,  0}, Percent); % green-ish
        2 -> gradient({0, 35, 35}, {255, 172, 5}, Percent); % green-ish
        3 -> gradient({255, 172, 5}, {255, 21, 21}, Percent); % yellow-ish
        4 -> gradient({255,118,  5}, {255,  5,  5}, Percent)
    end.


gradient({R, G, B}, {R2, G2, B2}, Percent) ->
    {shade(R, R2, Percent), shade(G, G2, Percent), shade(B, B2, Percent)}.

shade(X, Y, Percent) -> trunc(X + (Percent * (Y - X))).
