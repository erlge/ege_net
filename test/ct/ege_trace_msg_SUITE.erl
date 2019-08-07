%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ege_trace_msg_SUITE).

-include_lib("common_test/include/ct.hrl").
-include("ege_net.hrl").
-include("ege_net_ct.hrl").
-include("msg.hrl").
-include("msg_base.hrl").
-include("msg_base_pb.hrl").

-compile([export_all]).

all() -> [
%%    test,
%%    tcp_trace,
    ws_trace
].

ws_trace(_) ->
    process_flag(trap_exit, true),
    Ref = {ws, ?FUNCTION_NAME},
    {ok, Ref} = ege_net:start_listener(Ref, ?WS_LISTENER_OPTS(0)),
    Port = ranch:get_port(Ref),
    {ok, Client} = ege_ws_listener_SUITE:connect_do(Port),
    ok = ege_ws_listener_SUITE:receive_ok(Client, "/"),

    %% do
    ReqMsg = #heartbeat_req{id = 1},
    Binary = ?MSG_ROUTER_MODULE:encode_req_msg(ReqMsg),
    gun:ws_send(Client, {binary, Binary}),

    ege_ws_listener_SUITE:stop_all([Client]),
    ege_net:stop_listener(Ref),
    ok.

%%====================================================================
%% Internal functions
%%====================================================================