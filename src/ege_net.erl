%%%-------------------------------------------------------------------
%%% @doc
%%% 网络层对外api
%%% @end
%%%-------------------------------------------------------------------
-module(ege_net).

-include_lib("kernel/include/logger.hrl").
-include("ege_net.hrl").

%% API exports
-export([
    start_listeners/0,
    start_listeners/1,
    start_listener/2,
    stop_listeners/0, stop_listener/1,
    which_listeners_sup/0,
    online_count/0, online_count/1
]).

-type listener_type() :: ws | wss | tcp.
-type option() :: {port, listen_port()}
    | {num_acceptors, num_acceptors()}
    | {max_connections, max_connections()}
    | {certfile, tls_file()}
    | {keyfile, tls_file()}
    | gen_tcp:option().
%%-type option_name() :: num_acceptors | max_connections | certfile | keyfile | gen_tcp:option_name().
-type listen_port() :: pos_integer() | string().
-type num_acceptors() :: pos_integer()| string() | infinity.
-type max_connections() :: pos_integer()| string() | infinity.
-type tls_priv_file() :: priv | string().
-type tls_file() :: Filename :: string() | {AppName :: atom(), tls_priv_file()}.
-type trans_opts() :: [option(), ...].
-type proto_opts() :: any().
-type route_paths() :: cowboy_router:route_path().

-type listener_opts() ::
#{
    type := listener_type(), trans_opts := trans_opts(),
    proto_opts => proto_opts(), path => string(),
    route_paths => route_paths(), metrics_enable => boolean()
}.

%%====================================================================
%% API functions
%%====================================================================

-spec start_listeners() -> ok | {error, Reason :: any()}.
%% @doc 开启监听 默认从配置里面读取配置
start_listeners() ->
    Ports = application:get_env(?APP_NAME, ports, []),
    Listeners0 = application:get_env(?APP_NAME, listeners, []),
    Listeners = apply_ports(Ports, Listeners0),
    start_listeners(Listeners).
-spec start_listeners([{ranch:ref(), listener_opts()}, ...]) ->
    ok | {error, Reason :: any()}.
%% @doc 根据配置开启监听
start_listeners([{Ref, ListenerOpts} | Listeners]) ->
    case start_listener(Ref, ListenerOpts) of
        {ok, Ref} ->
            start_listeners(Listeners);
        Error -> Error
    end;
start_listeners([]) -> ok.

-spec start_listener(ranch:ref(), listener_opts()) ->
    {ok, Ref :: any()} | {error, Reason :: any()}.
start_listener(Ref, #{type := Type, trans_opts := TransOpts0} = ListenerOpts)
    when Type =:= ws orelse Type =:= wss ->
    case handle_trans_opts(TransOpts0, #{}) of
        {ok, TransOpts} ->
            MetricsEnable = maps:get(metrics_enable, ListenerOpts, false),
            Path = maps:get(path, ListenerOpts, "/"),
            ProtoOpts0 = maps:get(proto_opts, ListenerOpts, #{}),
            RoutePaths = maps:get(route_paths, ListenerOpts, []),
            ProtoOpts = path2proto_opts(Path, RoutePaths, ProtoOpts0, MetricsEnable),
            Res =
                case Type of
                    ws ->
                        cowboy:start_clear(Ref, TransOpts, ProtoOpts);
                    wss ->
                        cowboy:start_tls(Ref, TransOpts, ProtoOpts)
                end,
            case Res of
                {ok, _Sup} ->
                    ?LOG_DEBUG("start_listener start ~p, Ref:~p, TransOpts:~p, ProtoOpts:~p", [Type, Ref, TransOpts, ProtoOpts]),
                    {ok, Ref};
                Error -> Error
            end;
        Error -> Error
    end;
start_listener(Ref, #{type := tcp, trans_opts := TransOpts0} = ListenerOpts) ->
    case handle_trans_opts(TransOpts0, #{connection_type => worker}) of
        {ok, TransOpts} ->
            ProtoOpts = maps:get(proto_opts, ListenerOpts, []),
            case ranch:start_listener(Ref, ranch_tcp, TransOpts, ege_tcp_handler, ProtoOpts) of
                {ok, _Sup} ->
                    ?LOG_DEBUG("start_listener start tcp, Ref:~p, TransOpts:~p", [Ref, TransOpts]),
                    {ok, Ref};
                Error -> Error
            end;
        Error -> Error
    end.


-spec stop_listeners() -> [{ranch:ref(), true}, ...].
%% @doc 结束监听
stop_listeners() ->
    [{ListenerRef, stop_listener(ListenerRef)} || {ListenerRef, _} <- which_listeners_sup()].
-spec stop_listener(ranch:ref()) -> true.
stop_listener(ListenerRef) ->
    ok = ranch:stop_listener(ListenerRef).

-spec which_listeners_sup() -> [{ranch:ref(), Sup :: pid()}, ...].
which_listeners_sup() ->
    ranch_server:get_connections_sups().

-spec online_count() -> pos_integer().
%% @doc 所有在线数量
online_count() ->
    lists:foldl(fun({Ref, _}, Acc) -> online_count(Ref) + Acc end, 0, ranch_server:get_connections_sups()).
-spec online_count(ranch:ref()) -> pos_integer().
%% @doc 某个端口的在线数量
online_count(Ref) ->
    ConnsSup = ranch_server:get_connections_sup(Ref),
    proplists:get_value(active, supervisor:count_children(ConnsSup), 0).

%%====================================================================
%% Internal functions
%%====================================================================

%% 转换证书地址
trans_tls_file({AppName, Priv}, PrivDefault) ->
    PrivDir = code:priv_dir(AppName),
    CertFile0 =
        case Priv of
            priv -> PrivDefault;
            "priv" -> PrivDefault;
            CertFileT -> CertFileT
        end,
    CertFile = filename:join(PrivDir, CertFile0),
    case filelib:is_file(CertFile) of
        true ->
            {ok, CertFile};
        false ->
            {error, error_file}
    end;
trans_tls_file(File, _PrivDefault) ->
    case is_list(File) andalso filelib:is_file(File) of
        true ->
            {ok, File};
        false ->
            {error, error_file}
    end.

-spec handle_trans_opts(trans_opts(), map()) ->
    {ok, proplists:proplist()} | {error, any()}.
%% 端口
handle_trans_opts([{socket_opts, SocketOpts} | TransOpts], Acc) ->
    Port0 = proplists:get_value(port, SocketOpts),
    case to_integer(Port0) of
        Port when is_integer(Port) andalso Port >= 0 ->
            handle_trans_opts(TransOpts, Acc#{
                socket_opts => lists:keyreplace(port, 1, SocketOpts, {port, Port})
            });
        _ ->
            {error, {error_port, Port0}}
    end;
%% 监听进程个数
handle_trans_opts([{num_acceptors, NumAcceptors0} | TransOpts], Acc) ->
    case to_integer(NumAcceptors0) of
        NumAcceptors when is_integer(NumAcceptors) andalso NumAcceptors >= 0 ->
            handle_trans_opts(TransOpts, Acc#{num_acceptors => NumAcceptors});
        _ ->
            {error, {error_num_acceptors, NumAcceptors0}}
    end;
%% 最大连接数
handle_trans_opts([{max_connections, infinity} | TransOpts], Acc) ->
    handle_trans_opts(TransOpts, Acc#{max_connections => infinity});
handle_trans_opts([{max_connections, "infinity"} | TransOpts], Acc) ->
    handle_trans_opts(TransOpts, Acc#{max_connections => infinity});
handle_trans_opts([{max_connections, MaxConnections0} | TransOpts], Acc) ->
    case to_integer(MaxConnections0) of
        MaxConnections when is_integer(MaxConnections) andalso MaxConnections >= 0 ->
            handle_trans_opts(TransOpts, Acc#{max_connections => MaxConnections0});
        MaxConnections ->
            {error, {error_max_connections, MaxConnections}}
    end;
%% 证书-certfile
handle_trans_opts([{certfile, File0} | TransOpts], Acc) ->
    case trans_tls_file(File0, "ssl/server.crt") of
        {ok, File} ->
            handle_trans_opts(TransOpts, Acc#{certfile => File});
        {error, Reason} ->
            {error, {certfile, {Reason, File0}}}
    end;
%% 证书-keyfile
handle_trans_opts([{keyfile, File0} | TransOpts], Acc) ->
    case trans_tls_file(File0, "ssl/server.key") of
        {ok, File} ->
            handle_trans_opts(TransOpts, Acc#{keyfile => File});
        {error, Reason} ->
            {error, {keyfile, {Reason, File0}}}
    end;
%% other
handle_trans_opts([{Key, Value} | TransOpts], Acc) ->
    handle_trans_opts(TransOpts, Acc#{Key => Value});
handle_trans_opts([], TransOpts) ->
    case maps:find(socket_opts, TransOpts) of
        {ok, SocketOpts} ->
            case lists:keymember(port, 1, SocketOpts) of
                true ->
                    {ok, TransOpts};
                _ ->
                    {error, not_define_port}
            end;
        _ ->
            {error, not_define_port}
    end.

-spec path2proto_opts(Path :: string(), route_paths(), proto_opts(), MetricsEnable :: boolean()) -> map().
path2proto_opts(Path, RoutePaths, ProtoOpts0, MetricsEnable) ->
    Paths = [
        {Path, ege_ws_handler, []},
        {"/health_check", ege_health_check, []} | RoutePaths
    ],
    Dispatch = cowboy_router:compile([{'_', Paths}]),
    ProtoOpts = ProtoOpts0#{env => #{dispatch => Dispatch}},
    case MetricsEnable of
        true ->
            ProtoOpts#{
                metrics_callback => fun prometheus_cowboy2_instrumenter:observe/1,
                stream_handlers => [cowboy_metrics_h, cowboy_stream_h]
            };
        _ ->
            ProtoOpts
    end.

apply_ports(Ports, Listeners0) ->
    {NamePorts, Ports0} = lists:partition(fun({_Ref, _Port}) -> true;(_) -> false end, Ports),
    Listeners1 = lists:foldl(
        fun({Ref, Port}, ListenersAcc) ->
            case lists:keytake(Ref, 1, ListenersAcc) of
                {value, {Ref, #{trans_opts := TransOpts0} = ListenerOpts}, ListenersAcc0} ->
                    SocketOpts0 = proplists:get_value(socket_opts, TransOpts0, []),
                    SocketOpts = lists:keystore(port, 1, SocketOpts0, {port, Port}),
                    TransOpts = lists:keystore(socket_opts, 1, TransOpts0, {socket_opts, SocketOpts}),
                    [{Ref, ListenerOpts#{trans_opts => TransOpts}} | ListenersAcc0];
                _ ->
                    ListenersAcc
            end
        end, Listeners0, NamePorts),
    apply_ports(Ports0, Listeners1, []).

apply_ports([], OldListeners, Listeners) -> lists:sort(OldListeners ++ Listeners);
apply_ports(_, [], Listeners) -> lists:sort(Listeners);
apply_ports([Port | Ports] = OldPorts, [{Ref, #{trans_opts := TransOpts0} = ListenerOpts} = Listener | Listeners], Acc) ->
    SocketOpts0 = proplists:get_value(socket_opts, TransOpts0, []),
    case lists:keymember(port, 1, SocketOpts0) of
        true ->
            apply_ports(OldPorts, Listeners, [Listener | Acc]);
        false ->
            SocketOpts = [{port, Port} | SocketOpts0],
            TransOpts = lists:keystore(socket_opts, 1, TransOpts0, {socket_opts, SocketOpts}),
            apply_ports(Ports, Listeners, [{Ref, ListenerOpts#{trans_opts => TransOpts}} | Acc])
    end.

%% @doc Convert (almost) any value to an integer.
-spec to_integer(term()) -> integer().
to_integer(undefined) -> undefined;
to_integer([]) -> undefined;
to_integer(B) when is_binary(B) -> binary_to_integer(B);
to_integer(I) when is_integer(I) -> I;
to_integer(F) when is_float(F) -> erlang:round(F);
to_integer([C]) when is_integer(C) andalso (C > $9 orelse C < $0) -> C;
to_integer(L) when is_list(L) -> list_to_integer(L).