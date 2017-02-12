-module(debug).
-compile(export_all).

%
%   debug helper
%

% write `Str` to file
fout(Str) ->
    file:write_file("foo.txt", io_lib:fwrite("~p.\n", [Str]), [append]).