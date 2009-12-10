-module(mecab.sup).
-author("KONDO Takahiro <heartery@gmail.com>").

-behaviour(.supervisor).

-export([start_link/0, init/1]).

start_link() ->
    .supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    {ok, {{one_for_all, 10, 3600},
         [{server,
           {.mecab.server, start_link, []},
           permanent,
           2000,
           worker,
           [.mecab.server]}]}}.
