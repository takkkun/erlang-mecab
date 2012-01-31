What is erlang-mecab
====================

This module calls MeCab from Erlang.


Install
=======

    $ rake
    $ sudo rake install


How to use
==========

    $ erl +A 1
    > mecab:start().
    ok
    > mecab:version().
    "0.991"
    > Sentence = "こんにちは世界！".
    [12371,12435,12395,12385,12399,19990,30028,65281]
    > [io:format("~ts: ~ts~n", [S, F]) ||
       {S, F} <- mecab:parse_all(unicode:characters_to_binary(Sentence))].
    こんにちは: 感動詞,*,*,*,*,*,こんにちは,コンニチハ,コンニチワ
    世界: 名詞,一般,*,*,*,*,世界,セカイ,セカイ
    ！: 記号,一般,*,*,*,*,！,！,！
    [ok,ok,ok]
