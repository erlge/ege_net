-module(ege_ws_listener_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ege_net.hrl").
-include("ege_net_ct.hrl").

-compile([export_all]).

all() -> [
    start,
    start_port,
    connect,
    stop_connect,
    concurrency_connect,
    concurrency_stop_connect,
    get_status
].

start(_) ->
    Ref = {ws, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?WS_LISTENER_OPTS(0)),
    Listeners = ege_net:which_listeners_sup(),
    lists:keymember(Ref, 1, Listeners),
    ege_net:stop_listener(Ref).

start_port(_) ->
    Ref = {ws, ?FUNCTION_NAME},
    Port = 9912,
    {ok, Ref} = ege_net:start_listener(Ref, ?WS_LISTENER_OPTS(Port)),
    Port = ranch:get_port(Ref),
    Listeners = ege_net:which_listeners_sup(),
    lists:keymember(Ref, 1, Listeners),
    ege_net:stop_listener(Ref).

connect(_Config) ->
    Ref = {ws, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?WS_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {ok, Client} = connect_do(Port),
    ok = receive_ok(Client, "/"),
    1 = ege_net:online_count(Ref),
    stop_all([Client]),
    timer:sleep(10),
    0 = ege_net:online_count(Ref),
    ege_net:stop_listener(Ref),
    ok.

stop_connect(_Config) ->
    Ref = {ws, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?WS_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {ok, Client} = connect_do(Port),
    ok = receive_ok(Client, "/"),
    1 = ege_net:online_count(Ref),
    ege_net:stop_listener(Ref),
    receive_down([Client]),
    0 = ege_net:online_count(),
    ok.

concurrency_connect(_) ->
    Count = 100,
    Ref = {ws, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?WS_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {Pids, Count, 0} = connect(Port, "/", Count),
    Count = ege_net:online_count(Ref),
    stop_all(Pids),
    timer:sleep(50),
    0 = ege_net:online_count(Ref),
    ege_net:stop_listener(Ref),
    ok.

concurrency_stop_connect(_) ->
    Count = 100,
    Ref = {ws, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?WS_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {Pids, Count, 0} = connect(Port, "/", Count),
    Count = ege_net:online_count(Ref),
    ege_net:stop_listener(Ref),
    receive_down(Pids),
    0 = ege_net:online_count(),
    ok.

get_status(_) ->
    process_flag(trap_exit, true),
    Ref = {ws, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?WS_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {ok, Client} = connect_do(Port),
    ok = receive_ok(Client, "/"),

    %% do
    ConnSup = ranch_server:get_connections_sup(Ref),
    [{_, Proc, _, _}] = supervisor:which_children(ConnSup),
    {status, _, {module, _}, [_, running, _, _, _]} = sys:get_status(Proc),

    stop_all([Client]),
    ege_net:stop_listener(Ref),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

connect_do(Port) ->
%%    {ok, ConnPid} = gun:open("127.0.0.1", Port, #{retry=>0}),
    {ok, ConnPid} = gun:open("127.0.0.1", Port),
    monitor(process, ConnPid),
    {ok, ConnPid}.

receive_ok(ConnPid, Path) ->
    receive
        {gun_up, ConnPid, _} ->
            StreamRef = gun:ws_upgrade(ConnPid, Path, []),
            receive
                {gun_upgrade, ConnPid, StreamRef, _, _Headers} ->
                    ok
            after 1000 ->
                ct:pal("~p", [receive_all()]),
                gun_ws_upgrade_fail
            end
    after 1000 -> gun_up_fail
    end.

receive_all() ->
    receive
        Msg -> [Msg|receive_all()]
    after 0 -> []
    end.

connect(Port, Path, Count) ->
    Clients = [connect_do(Port) || _ <- lists:duplicate(Count, ok)],
    lists:foldl(
        fun({ok, Client}, {List, Succ, Fail}) ->
            case receive_ok(Client, Path) of
                ok ->
                    {[Client | List], Succ + 1, Fail};
                fail ->
                    {List, Succ, Fail + 1}
            end
        end, {[], 0, 0}, Clients).

receive_down(Pids) when is_list(Pids) ->
    receive_down_do(Pids).

receive_down_do([]) -> ok;
receive_down_do([{ok, ConnPid} | Pids]) ->
    receive_down_do([ConnPid | Pids]);
receive_down_do([ConnPid | Pids]) ->
    receive
        {'DOWN', _MRef, process, ConnPid, _Reason} ->
            gun:shutdown(ConnPid),
            receive_down_do(Pids);
        {gun_down, ConnPid, _, _Reason, _, _} ->
            gun:shutdown(ConnPid),
            receive_down_do(Pids)
    after 500 ->
        error(ws_closed_timeout)
    end.

stop_all(Pids) ->
    [stop(Client) || Client <- Pids],
    receive_down(Pids).

stop({ok, Client}) ->
    gun:shutdown(Client);
stop(Client) ->
    gun:shutdown(Client).