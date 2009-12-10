-module(mecab.app).
-author("KONDO Takahiro <heartery@gmail.com>").

-behaviour(.application).

-export([start/2, stop/1]).

start(_, _) ->
    sup:start_link().

stop(_) ->
    ok.
