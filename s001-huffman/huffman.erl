-module(huffman).
-compile(export_all).

sample() -> "the quick brown fox jumps over the lazy dog
    this is a sample text that we will use when we build
    up a table we will only handle lower case letters and
    no punctuation symbols the frequency will of course not
    represent english but it is probably not that far off".

text() -> "this is something that we should encode".

test() ->
    Sample = sample(),
    Tree = tree(Sample),
    Encode = encode_table(Tree),
    Decode = decode_table(Tree),
    Text = text(),
    Seq = encode(Text, Encode),
    Text = decode(Seq, Decode).



% tree(_Sample) -> na.
% encode_table(_Tree) -> na.
% decode_table(_Tree) -> na.
% encode(_Text, _Table) -> na.
% decode(_Seq, _Table) -> na.