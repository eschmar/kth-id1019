-module(testing).
-compile(export_all).

%
%   Small tests
%

customAnd(false, _) -> 
    false;
customAnd(_, false) -> 
    false;
customAnd(true, true) -> 
    true.

index(0, [X|_]) -> X;
index(N,[_|Xs]) when N>0 -> index(N-1,Xs).

tryCatch() ->
    X=2,
    try (X=3) of
        Val -> {normal, Val}
    catch
        error:Error -> {error, Error}
    end.
