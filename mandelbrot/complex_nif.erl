-module(complex_nif).
-export([depth/3]).
-on_load(init/0).

init() ->
    ok = erlang:load_nif("./complex_nif", 0).

% I: Iteration
% Z: Complex number Z
% C: Original complex number
% M: Max depth
depth(_CR, _CI, _M) ->
    exit(nif_library_not_loaded).
