-module(brot).

-export([start/1, brot/2, mandelbrot/2]).

start(Mandel) ->
    {ok, spawn(fun() -> init(Mandel) end)}.

init(Mandel) ->
    Self = self(),
    brot(Mandel, Self).

brot(Mandel, Self) ->
    Mandel ! {req, Self},
    receive
	{pos, Pr, Pos, C, M} ->
	    I = mandelbrot(C, M),
	    Pr ! {res, Pos, I},
	    brot(Mandel, Self);
	done ->
	    ok
    end.

%%% mandel(C,M) : calculate the mandelbrot value of complex value C
%%% with a maximum iteration of M. Returns 0..(M-1)

mandelbrot(C, M) ->
    Z0 = cmplx:new(0,0),
    test(0, Z0, C, M).

test(M, _Z, _C, M) ->
    0;
test(I, Z, C, M) ->
    A = cmplx:abs(Z),
    if 
	A < 2.0 ->
	    io:format("abs ~w~n", [A]),
	    Z1 = cmplx:add(cmplx:sqr(Z), C),
	    test(I+1, Z1, C, M);
	true ->
	    io:format("final abs ~w~n", [A]),
	    I
    end.


    
    
		  



