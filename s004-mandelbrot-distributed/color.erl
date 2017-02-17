-module(color).

-export([convert/2, convert2/2]).


%% Convert a scalar, from 0 to max, to a suitabe color represented as
%% {R,G,B} where each element is 0..255. This is just one way of doing
%% it, there are more advanced ways of doing this so do experiment.


convert(D, Max) ->
    F = D/Max,
    A = F*4,              %% A is [0 - 4.0]
    X = trunc(A),         %% X is [0,1,2,3,4]
    Y = trunc(255*(A-X)), %% Y is [0 - 255]
    case X of
	0 ->
	    {Y, 0, 0};       %% black -> red
	1 ->
	    {255, Y, 0};     %% red -> yellow
	2 ->
	    {255-Y,255, 0};  %% yellow -> green
	3 ->
	    {0, 255, Y};     %% green -> light blue
	4 ->
	    {0,255-Y,255}    %% light blue -> blue
    end.


convert2(D, Max) ->
    I = trunc(math:pow(Max, 1/3)),
    R = D rem I,
    D1 = trunc(D / I),
    G = D1 rem I,
    D2 = trunc(D1 / I),
    B = D2,
    {R,G,B}.
