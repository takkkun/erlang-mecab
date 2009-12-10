-module(mecab).
-author("KONDO Takahiro <heartery@gmail.com>").

-export([start/0, stop/0]).
-export([parse/1, parse/2, parse_all/1, parse_all/2,
         version/0, dic_info/0, dic_info/1]).

start() ->
    application:start(?MODULE).

stop() ->
    application:stop(?MODULE).

parse(Sentence) ->
    parse(Sentence, []).

parse(Sentence, Options) ->
    mecab.server:parse(Sentence, Options).

parse_all(Sentence) ->
    parse_all(Sentence, []).

parse_all(Sentence, Options) ->
    case parse(Sentence, Options) of
        {ok, MeCabId}   -> mecab.server:nodes(MeCabId);
        {error, Reason} -> {error, Reason}
    end.

version() ->
    mecab.server:version().

dic_info() ->
    dic_info(default).

dic_info(Path) ->
    mecab.server:dic_info(Path).
