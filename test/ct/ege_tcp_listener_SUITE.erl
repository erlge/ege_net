-module(ege_tcp_listener_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ege_net.hrl").
-include("ege_net_ct.hrl").

-compile([export_all]).

all() -> [
    start,
    start_port,
    connect,
    connect_set_packet,
    stop_connect,
    concurrency_connect,
    concurrency_stop_connect,
    not_supervisor
].

start(_) ->
    Ref = {tcp, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?TCP_LISTENER_OPTS(0)),
    Listeners = ege_net:which_listeners_sup(),
    lists:keymember(Ref, 1, Listeners),
    ege_net:stop_listener(Ref).

start_port(_) ->
    Ref = {tcp, ?FUNCTION_NAME},
    Port = 9912,
    {ok, Ref} = ege_net:start_listener(Ref, ?TCP_LISTENER_OPTS(Port)),
    Port = ranch:get_port(Ref),
    Listeners = ege_net:which_listeners_sup(),
    lists:keymember(Ref, 1, Listeners),
    ege_net:stop_listener(Ref).

connect(_Config) ->
    process_flag(trap_exit, true),
    Ref = {tcp, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?TCP_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {ok, Client} = connect_do(self(), Port),
    timer:sleep(10),
    1 = ege_net:online_count(Ref),
    stop_all([Client]),
    timer:sleep(20),
    0 = ege_net:online_count(Ref),
    ege_net:stop_listener(Ref),
    ok.

connect_set_packet(_Config) ->
    process_flag(trap_exit, true),
    Ref = {tcp, ?FUNCTION_NAME},
    TcpListenerOpts0 = ?TCP_LISTENER_OPTS(0),
    TcpListenerOpts = TcpListenerOpts0#{proto_opts => [{packet, 4}]},
    {ok, Ref} = ege_net:start_listener(Ref, TcpListenerOpts),
    Port = ranch:get_port(Ref),
    {ok, Client} = connect_do(self(), Port, 4),
    timer:sleep(10),
    1 = ege_net:online_count(Ref),
    stop_all([Client]),
    timer:sleep(20),
    0 = ege_net:online_count(Ref),
    ege_net:stop_listener(Ref),
    ok.

stop_connect(_Config) ->
    process_flag(trap_exit, true),
    Ref = {tcp, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?TCP_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {ok, Client} = connect_do(self(), Port),
    timer:sleep(10),
    1 = ege_net:online_count(Ref),
    ege_net:stop_listener(Ref),
    receive_exit([Client]),
    timer:sleep(10),
    0 = ege_net:online_count(),
    ok.

concurrency_connect(_) ->
    process_flag(trap_exit, true),
    Parent = self(),
    Count = 1000,
    Ref = {tcp, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?TCP_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {Pids, Count, 0} = connect(Parent, Port, Count),
    timer:sleep(10),
    Count = ege_net:online_count(Ref),
    stop_all(Pids),
    timer:sleep(10),
    0 = ege_net:online_count(Ref),
    ege_net:stop_listener(Ref),
    ok.

concurrency_stop_connect(_) ->
    process_flag(trap_exit, true),
    Parent = self(),
    Count = 1000,
    Ref = {tcp, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?TCP_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {Pids, Count, 0} = connect(Parent, Port, Count),
    timer:sleep(10),
    Count = ege_net:online_count(Ref),
    ege_net:stop_listener(Ref),
    receive_exit(Pids),
    timer:sleep(10),
    0 = ege_net:online_count(),
    ok.

not_supervisor(_) ->
    process_flag(trap_exit, true),
    Ref = {tcp, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?TCP_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {ok, Client} = connect_do(self(), Port),
    timer:sleep(10),

    %% do
    ConnSup = ranch_server:get_connections_sup(Ref),
    [{_, _, worker, _}] = supervisor:which_children(ConnSup),

    stop_all([Client]),
    ege_net:stop_listener(Ref),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

connect_do(Parent, Port) ->
    connect_do(Parent, Port, 2).
connect_do(Parent, Port, Packet) ->
    Ref = erlang:make_ref(),
    Pid = spawn_link(
        fun() ->
            case gen_tcp:connect("localhost", Port, [binary, {packet, Packet}], 1000) of
                {ok, Socket} ->
                    Parent ! {succ, Ref},
                    wait_stop(Socket);
                Reason ->
                    Parent ! {fail, Reason}
            end
        end),
    receive
        {succ, Ref} -> {ok, Pid};
        {fail, Reason} -> {error, Reason}
    end.

wait_stop(Socket) ->
    receive
        {tcp_closed, Socket} ->
            ok;
        closed ->
            gen_tcp:close(Socket),
            ok
    end.

connect(Parent, Port, Count) ->
    lists:foldl(
        fun(_, {List, Succ, Fail}) ->
            case connect_do(Parent, Port) of
                {ok, Client} ->
                    {[Client | List], Succ + 1, Fail};
                _ ->
                    {List, Succ, Fail + 1}
            end
        end, {[], 0, 0}, lists:duplicate(Count, ok)).

receive_exit(Pids) when is_list(Pids) ->
    receive_exit_do(Pids).

receive_exit_do([]) -> ok;
receive_exit_do([{ok, From} | Pids]) ->
    receive_exit_do([From | Pids]);
receive_exit_do([From | Pids]) ->
    receive
        {'EXIT', From, normal} ->
            receive_exit_do(Pids)
    after 500 ->
        error(tcp_closed_timeout)
    end.

stop_all(Pids) ->
    [Pid ! closed || Pid <- Pids],
    receive_exit(Pids).