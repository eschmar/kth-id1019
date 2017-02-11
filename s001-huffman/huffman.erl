-module(huffman).
-compile(export_all).

%
%   huffman coding
%

init() ->
    code:add_path("../helper"),
    code:add_path("../introduction").

sample() -> "the quick brown fox jumps over the lazy dog
    this is a sample text that we will use when we build
    up a table we will only handle lower case letters and
    no punctuation symbols the frequency will of course not
    represent english but it is probably not that far off".

text() -> "this is something that we should encode".

test() ->
    init(),
    Sample = sample(),
    color:out("Pew", magenta),
    Tree = tree(Sample),
    Encode = encode_table(Tree),
    Text = text(),
    Seq = encode(Text, Encode),
    Text = decode(Seq, Tree).

%
%   extract frequency of characters
%

freq(Sample) -> freq(Sample, []).

freq([], Freq) ->
    quicksort(Freq);
freq([Char | Rest], Freq) ->
    freq(Rest, freq_insert(Char, Freq)).

freq_insert(Char, []) -> [{Char, 1}];
freq_insert(Char, [Current | Tail]) ->
    {Index, Count} = Current,
    if
        Index == Char -> [{Char, (Count + 1)} | Tail];
        true -> [Current | freq_insert(Char, Tail)]
    end.

%
%   frequency quick sort
%

append(X, []) -> [X];
append(X, [Head | Tail]) -> [Head | append(X, Tail)].

quicksort([]) -> [];
quicksort([Pivot | Tail]) ->
    {Left, Right} = quick_split(Pivot, Tail, [], []),
    quicksort(Left) ++ [Pivot] ++ quicksort(Right).

quick_split(_, [], Left, Right) ->
    {Left, Right};
quick_split(Pivot, [Current | Tail], Left, Right) ->
    {_, A} = Pivot,
    {_, B} = Current,
    if
        B < A -> quick_split(Pivot, Tail, append(Current, Left), Right);
        true -> quick_split(Pivot, Tail, Left, append(Current, Right))
    end.

%
%   huffman tree
%

tree(Sample) ->
    color:out("Extracting char frequencies", cyan),
    Freq = freq(Sample),

    color:out("Build huffman tree", green),
    Tree = tree_build(Freq).

tree_build([{Tree, _} | []]) ->
    Tree;
tree_build(Freq) ->
    [First | [Next | Tail]] = Freq,
    {A, AF} = First,
    {B, BF} = Next,
    tree_build(quicksort([{{A, B}, AF+BF} | Tail])).

%
%   encoding table
%

encode_table(Tree) -> encode_table(Tree, []).

encode_table({A, B}, Partial) ->
    encode_table(A, Partial ++ [0]) ++ encode_table(B, Partial ++ [1]);
encode_table(Element, Partial) ->
    [{Element, Partial}].

%
%   encode
%

lookup(_, []) -> false;
lookup(X, [{Current, Sequence} | Tail]) ->
    case Current of
        X -> Sequence;
        _ -> lookup(X, Tail)
    end.

encode(Text, Table) -> encode(Text, Table, []).

encode([], _, Sequence) -> Sequence;
encode([Char | Tail], Table, Sequence) ->
    encode(Tail, Table, Sequence ++ lookup(Char, Table)).

%
%   encode
%

decode([], _) -> [];
decode(Sequence, Tree) -> decode(Sequence, Tree, []).
decode(Sequence, Tree, Result) -> pew.

decode_char(Char, Tree) -> pew.


%
%   helper
%

fout(Str) ->
    file:write_file("foo.txt", io_lib:fwrite("~p.\n", [Str]), [append]).