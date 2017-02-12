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
    end.