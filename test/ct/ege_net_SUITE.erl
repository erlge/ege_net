%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ege_net_SUITE).
-include_lib("common_test/include/ct.hrl").
-include("ege_net.hrl").
-include("ege_net_ct.hrl").

-compile([export_all]).

all() -> [
    listeners,
    send_to_group
].

listeners(_) ->
    ListenerOpts = ?WS_LISTENER_OPTS(0),
    application:set_env(ege_net, listeners, [{?FUNCTION_NAME, ListenerOpts}]),
    ege_net:start_listeners(),

    ege_net:stop_listeners(),
    ok.

send_to_group(_) ->
    NetHandler = #net_handler{},
    ege_net_handler:handler_init(NetHandler),

    ege_net_handler:loop_check_handler(),
    receive
        {check_tick, _} -> ok
    after 100 -> error(send_to_group)
    end.