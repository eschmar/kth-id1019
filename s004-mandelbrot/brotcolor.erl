-module(brotcolor).
-compile(export_all).

%
%   brotcolor - iteration to color converter
%

convert(Depth, Max) ->
    Fraction = (Depth / Max) * 4,
    Section = trunc((Fraction)) rem 5,
    Offset = trunc(255 * (Fraction - Section)),
    case Section of
        0 -> {Offset, 0, 0};
        1 -> {255, Offset, 0};
        2 -> {255 - Offset, 255, 0};
        3 -> {0, 255, Offset};
        4 -> {0, 255 - Offset, 255}
    end.