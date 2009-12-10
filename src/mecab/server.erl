-module(mecab.server).
-author("KONDO Takahiro <heartery@gmail.com>").

-export([start_link/0, parse/2, nodes/1, version/0, dic_info/1]).

-behaviour(.gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {port    = open_port({spawn, mecab_drv}, [binary]),
                clients = .dict:new()}).

-define(PARSE,    1).
-define(VERSION,  2).
-define(DIC_INFO, 3).

start_link() ->
    .gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

parse(Sentence, Options) ->
    .gen_server:call(?MODULE, {parse, Sentence, Options}).

nodes(MeCabId) ->
    collect_nodes(MeCabId, []).

collect_nodes(MeCabId, Nodes) ->
    receive
        {mecab, MeCabId, Result} ->
            case Result of
                {node, Node}    -> collect_nodes(MeCabId, [Node|Nodes]);
                bos_eos         -> collect_nodes(MeCabId, [bos_eos|Nodes]);
                complete        -> .lists:reverse(Nodes);
                {error, Reason} -> {error, Reason}
            end
    after 3000 ->
            {error, timeout}
    end.

version() ->
    .gen_server:call(?MODULE, version).

dic_info(Path) ->
    .gen_server:call(?MODULE, {dic_info, Path}).

%%% callback functions

init([]) ->
    process_flag(trap_exit, true),
    Dir = .code:priv_dir(mecab),
    case load_driver([Dir, "lib", .erlang:system_info(system_architecture)]) of
        ok ->
            {ok, #state{}};
        _ ->
            case load_driver([Dir, "lib"]) of
                ok              -> {ok, #state{}};
                {error, Reason} -> {stop, Reason}
            end
    end.

handle_call({parse, Sentence, Options}, From, #state{port    = Port,
                                                     clients = Clients} = State) ->
    case control(Port, ?PARSE, {make_ref(), Sentence, Options}) of
        {ok, MeCabId} ->
            {reply, {ok, MeCabId}, State#state{clients = Clients:store(MeCabId, From)}};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call(version, _, #state{port = Port} = State) ->
    {reply, control(Port, ?VERSION), State};

handle_call({dic_info, Path}, _, #state{port = Port} = State) ->
    {reply, control(Port, ?DIC_INFO, Path), State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info({Port, {data, Reply}}, #state{port = Port, clients = Clients} = State) ->
    {noreply, case binary_to_term(Reply) of
                  {mecab, MeCabId, Result} ->
                      case Clients:find(MeCabId) of
                          {ok, Client} ->
                              reply(Client, MeCabId, Result),
                              State#state{clients = case Result of
                                                        complete   -> Clients:erase(MeCabId);
                                                        {error, _} -> Clients:erase(MeCabId);
                                                        _          -> Clients
                                                    end};
                          error ->
                              State
                      end;
                  _ ->
                      State
              end};

handle_info({'EXIT', Port, Reason}, #state{port = Port} = State) ->
    {stop, {port_died, Reason}, State}.

terminate(Reason, #state{port = Port, clients = Clients}) ->
    [reply(Client, MeCabId, {error, Reason}) || {MeCabId, Client} <- Clients:to_list()],
    port_close(Port).

code_change(_, State, _) ->
    {ok, State}.

%%% private functions

load_driver(Path) ->
    case .erl_ddll:load_driver(.filename:join(Path), mecab_drv) of
        ok                      -> ok;
        {error, already_loaded} -> ok;
        Error                   -> Error
    end.

control(Port, Operation) ->
    binary_to_term(port_control(Port, Operation, <<>>)).

control(Port, Operation, Data) ->
    binary_to_term(port_control(Port, Operation, term_to_binary(Data))).

reply({Pid, _}, MeCabId, Result) ->
    Pid ! {mecab, MeCabId, Result}.
