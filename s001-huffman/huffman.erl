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

test() -> huffman(sample(), text()).

test2() ->
    Sample = read("foo.txt", 9999),
    Text = read("foo.txt", 58),
    huffman(Sample, Text).

huffman(Sample, Text) ->
    init(),
    Tree = tree(Sample),
    Encode = encode_table(Tree),

    color:out("Source:", magenta),
    color:out(Text, green),

    Seq = encode(Text, Encode),
    color:out("Encoded sequence:", magenta),
    erlang:display(Seq),

    color:out("Decode:", magenta),
    Decoded = decode(Seq, Tree),
    color:out(Decoded, cyan).

read(File, N) ->
    {ok, Fd} = file:open(File, [read, binary]),
    {ok, Binary} = file:read(Fd, N),
    file:close(Fd),
    case unicode:characters_to_list(Binary, utf8) of
        {incomplete, List, _} ->
            List;
        List ->
            List
    end.

%
%   extract frequency of characters
%

freq(Sample) -> freq(Sample, []).

freq([], Freq) ->
    insertionsort(Freq);
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
%   insertion sort
%

insert(Element, []) -> [Element];
insert(Element, [Current | Tail]) ->
    {_, A} = Element,
    {_, B} = Current,
    if
        B > A -> [Current | insert(Element, Tail)];
        true -> [Element | [Current | Tail]]
    end.

insertionsort(List) -> list:reverse(insertionsort_acc(List, [])).
insertionsort_acc([], List) -> List;
insertionsort_acc([Current | Tail], Sorted) -> insertionsort_acc(Tail, insert(Current, Sorted)).

%
%   huffman tree
%

tree(Sample) ->
    color:out("Extracting char frequencies", cyan),
    Freq = freq(Sample),

    color:out("Build huffman tree", green),
    tree_build(Freq).

tree_build([{Tree, _} | []]) ->
    Tree;
tree_build(Freq) ->
    fout(Freq),
    [First | [Next | Tail]] = Freq,
    {A, AF} = First,
    {B, BF} = Next,
    tree_build(insertionsort([{{A, B}, AF+BF} | Tail])).

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
%   decode
%

decode([], _) -> [];
decode(Sequence, Tree) -> decode(Sequence, [], Tree, Tree).

decode([], Result, _, Char) ->  Result ++ [Char];
decode([Bit | Sequence], Result, Tree, {Left, Right}) ->
    case Bit of
        0 -> decode(Sequence, Result, Tree, Left);
        1 -> decode(Sequence, Result, Tree, Right)
    end;
decode(Sequence, Result, Tree, Char) ->
    decode(Sequence, Result ++ [Char], Tree, Tree).

%
%   helper
%

fout(Str) ->
    file:write_file("foo.txt", io_lib:fwrite("~p.\n", [Str]), [append]).