%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ege_ws_handler_tests).
-include("ege_net.hrl").
-include_lib("eunit/include/eunit.hrl").

dispatch_msg_test_() ->
    OldState = old_state,
    meck:new(ege_msg, [passthrough]),

    %% ping
    PingReturn = ege_ws_handler:websocket_handle({ping, <<>>}, OldState),
    %% nothing
    meck:expect(ege_msg, dispatch_msg, 2, {ok, OldState}),
    NothingReturn = ege_ws_handler:websocket_handle({binary, <<>>}, OldState),
    %% state
    meck:expect(ege_msg, dispatch_msg, 2, {ok, new_state}),
    StateReturn = ege_ws_handler:websocket_handle({binary, <<>>}, OldState),
    %% resp_bin_list one
    meck:expect(ege_msg, dispatch_msg, 2, {ok, [<<"test00">>], OldState}),
    RespBinaryReturn1 = ege_ws_handler:websocket_handle({binary, <<>>}, OldState),
    %% resp_bin_list list
    meck:expect(ege_msg, dispatch_msg, 2, {ok, [<<"test01">>, <<"test02">>], OldState}),
    RespBinaryReturn2 = ege_ws_handler:websocket_handle({binary, <<>>}, OldState),
    %% stop
    meck:expect(ege_msg, dispatch_msg, 2, {stop, error, OldState}),
    StopReturn = ege_ws_handler:websocket_handle({binary, <<>>}, OldState),
    %% stop & resp_bin_list one
    meck:expect(ege_msg, dispatch_msg, 2, {stop, error, [<<"test03">>], OldState}),
    StopReturn2 = ege_ws_handler:websocket_handle({binary, <<>>}, OldState),
    %% stop & resp_bin_list list
    meck:expect(ege_msg, dispatch_msg, 2, {stop, error, [<<"test04">>, <<"test05">>], OldState}),
    StopReturn3 = ege_ws_handler:websocket_handle({binary, <<>>}, OldState),

    meck:unload(ege_msg),

    %% err_lvl_up
    Now = erlang:system_time(second),
    NowMs = erlang:system_time(millisecond),
    NetHandler = #net_handler{last_cmd_time = Now, last_heartbeat = NowMs},
    IsStopReturn = ege_ws_handler:websocket_info({err_lvl_up, ?E_CRITICAL, error}, NetHandler),
    IsStopReturn2 = ege_ws_handler:websocket_info({err_lvl_up, ?E_DEBUG, error}, NetHandler),

    %% check_tick
    CheckTickReturn = ege_ws_handler:websocket_info({check_tick, NowMs + ?HEARTBEAT_KICKOUT_TIMEOUT + 1000}, NetHandler),
    CheckTickReturn2 = ege_ws_handler:websocket_info({check_tick, NowMs}, NetHandler),

    [
        ?_assertEqual({reply, {pong, <<>>}, OldState}, PingReturn),
        ?_assertEqual({ok, OldState}, NothingReturn),
        ?_assertEqual({ok, new_state}, StateReturn),
        ?_assertEqual({reply, [{binary, <<"test00">>}], OldState}, RespBinaryReturn1),
        ?_assertEqual({reply, [{binary, <<"test01">>}, {binary, <<"test02">>}], OldState}, RespBinaryReturn2),
        ?_assertEqual({stop, OldState}, StopReturn),
        ?_assertEqual({reply, [{binary, <<"test03">>}, {close, 1000, <<>>}], OldState}, StopReturn2),
        ?_assertEqual({reply, [{binary, <<"test04">>}, {binary, <<"test05">>}, {close, 1000, <<>>}], OldState}, StopReturn3),
        ?_assertEqual({stop, NetHandler}, IsStopReturn),
        ?_assertEqual({ok, NetHandler#net_handler{err_times = ?E_DEBUG}}, IsStopReturn2),
        ?_assertEqual({stop, NetHandler}, CheckTickReturn),
        ?_assertEqual({ok, NetHandler}, CheckTickReturn2)
    ].

handle_send_test_() ->
    OldState = old_state,
    Return1 = ege_ws_handler:websocket_info({send, <<"test00">>}, OldState),
    Return2 = ege_ws_handler:websocket_info({send, [<<"test01">>, <<"test02">>]}, OldState),
    Return3 = ege_ws_handler:websocket_info({send, []}, OldState),
    [
        ?_assertEqual({reply, {binary, <<"test00">>}, OldState}, Return1),
        ?_assertEqual({reply, [{binary, <<"test01">>}, {binary, <<"test02">>}], OldState}, Return2),
        ?_assertEqual({ok, OldState}, Return3)
    ].

handle_stop_test_() ->
    OldState = old_state,
    StopReturn1 = ege_ws_handler:websocket_info(stop, OldState),
    ExitReturn = ege_ws_handler:websocket_info({'EXIT', pid, normal}, OldState),
    StopReturn2 = ege_ws_handler:websocket_info({stop, <<"test">>}, OldState),
    [
        ?_assertEqual({stop, OldState}, StopReturn1),
        ?_assertEqual({stop, OldState}, ExitReturn),
        ?_assertEqual({reply, [{binary, <<"test">>}, {close, 1000, <<>>}], OldState}, StopReturn2)
    ].