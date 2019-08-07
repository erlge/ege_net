%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ege_tcp_handler_tests).
-include("ege_net.hrl").
-include_lib("eunit/include/eunit.hrl").
-record(state, {socket, sub_state}).

dispatch_msg_test_() ->
    meck:new(gen_tcp, [unstick, passthrough]),
    meck:new(inet, [unstick, passthrough]),
    meck:new(ege_msg, [passthrough]),
    meck:expect(gen_tcp, send, fun(S, B) -> S ! {gen_tcp, B} end),
    meck:expect(inet, setopts, 2, ok),
    OldState = #state{socket = self(), sub_state = old_state},

    %% nothing
    meck:expect(ege_msg, dispatch_msg, 2, {ok, old_state}),
    NothingReturn = ege_tcp_handler:handle_info({tcp, self(), <<>>}, OldState),
    %% state
    meck:expect(ege_msg, dispatch_msg, 2, {ok, new_state}),
    StateReturn = ege_tcp_handler:handle_info({tcp, self(), <<>>}, OldState),
    %% resp_bin_list one
    meck:expect(ege_msg, dispatch_msg, 2, {ok, [<<"test00">>], old_state}),
    RespBinaryReturn1 = ege_tcp_handler:handle_info({tcp, self(), <<>>}, OldState),
    receive
        {gen_tcp, RespBinary1} -> RespBinary1
    end,
    %% resp_bin_list list
    meck:expect(ege_msg, dispatch_msg, 2, {ok, [<<"test01">>, <<"test02">>], old_state}),
    RespBinaryReturn2 = ege_tcp_handler:handle_info({tcp, self(), <<>>}, OldState),
    RespBinaryList = receive_list(2),
    %% stop
    meck:expect(ege_msg, dispatch_msg, 2, {stop, error, new_state}),
    StopReturn1 = ege_tcp_handler:handle_info({tcp, self(), <<>>}, OldState),
    meck:expect(ege_msg, dispatch_msg, 2, {stop, error, <<"test_stop">>, old_state}),
    StopReturn2 = ege_tcp_handler:handle_info({tcp, self(), <<"test_stop">>}, OldState),
    receive
        {gen_tcp, StopRespBinary} -> StopRespBinary
    end,

    meck:unload([gen_tcp, inet, ege_msg]),

    %% err_lvl_up
    IsStopReturn = ege_tcp_handler:handle_info({err_lvl_up, ?E_CRITICAL, error}, OldState),
    Now = erlang:system_time(second),
    NowMs = erlang:system_time(millisecond),
    NetHandler = #net_handler{last_cmd_time = Now, last_heartbeat = NowMs},
    IsStopReturn2 = ege_tcp_handler:handle_info({err_lvl_up, ?E_DEBUG, error}, OldState#state{sub_state = NetHandler}),

    %% check_tick
    CheckTickReturn = ege_tcp_handler:handle_info({check_tick, NowMs + ?HEARTBEAT_KICKOUT_TIMEOUT + 1000},
        OldState#state{sub_state = NetHandler}),
    CheckTickReturn2 = ege_tcp_handler:handle_info({check_tick, NowMs}, OldState#state{sub_state = NetHandler}),

    [
        ?_assertEqual({noreply, OldState}, NothingReturn),
        ?_assertEqual({noreply, #state{socket = self(), sub_state = new_state}}, StateReturn),
        ?_assertEqual({noreply, OldState}, RespBinaryReturn1),
        ?_assertEqual(<<"test00">>, RespBinary1),
        ?_assertEqual({noreply, OldState}, RespBinaryReturn2),
        ?_assertEqual([<<"test01">>, <<"test02">>], RespBinaryList),

        ?_assertEqual({stop, normal, OldState#state{sub_state = new_state}}, StopReturn1),
        ?_assertEqual({stop, normal, OldState}, StopReturn2),
        ?_assertEqual(<<"test_stop">>, StopRespBinary),

        ?_assertEqual({stop, normal, OldState}, IsStopReturn),
        ?_assertEqual({noreply, OldState#state{sub_state = NetHandler#net_handler{err_times = ?E_DEBUG}}}, IsStopReturn2),
        ?_assertEqual({stop, normal, OldState#state{sub_state = NetHandler}}, CheckTickReturn),
        ?_assertEqual({noreply, OldState#state{sub_state = NetHandler}}, CheckTickReturn2)
    ].

receive_list(0) -> [];
receive_list(N) ->
    receive
        {gen_tcp, RespBinary1} ->
            [RespBinary1 | receive_list(N - 1)]
    end.

handle_send_test_() ->
    meck:new(gen_tcp, [unstick, passthrough]),
    meck:expect(gen_tcp, send, fun(S, B) -> S ! {gen_tcp, B} end),

    OldState = #state{socket = self(), sub_state = old_state},
    Return1 = ege_tcp_handler:handle_info({send, <<"test00">>}, OldState),
    receive
        {gen_tcp, RespBinary} -> RespBinary
    end,
    Return2 = ege_tcp_handler:handle_info({send, [<<"test01">>, <<"test02">>]}, OldState),
    RespBinaryList = receive_list(2),

    meck:unload(gen_tcp),
    [
        ?_assertEqual({noreply, OldState}, Return1),
        ?_assertEqual(<<"test00">>, RespBinary),
        ?_assertEqual({noreply, OldState}, Return2),
        ?_assertEqual([<<"test01">>, <<"test02">>], RespBinaryList)
    ].

handle_stop_test_() ->
    meck:new(gen_tcp, [unstick, passthrough]),
    meck:expect(gen_tcp, send, fun(S, B) -> S ! {gen_tcp, B} end),

    OldState = #state{socket = self(), sub_state = old_state},
    StopReturn1 = ege_tcp_handler:handle_info(stop, OldState),
    TcpClosedReturn = ege_tcp_handler:handle_info({tcp_closed, s}, OldState),
    ExitReturn = ege_tcp_handler:handle_info({'EXIT', pid, normal}, OldState),
    TimeoutReturn = ege_tcp_handler:handle_info(timeout, OldState),
    StopReturn2 = ege_tcp_handler:handle_info({stop, <<"test">>}, OldState),
    receive
        {gen_tcp, RespBinary} -> RespBinary
    end,

    meck:unload(gen_tcp),
    [
        ?_assertEqual({stop, normal, OldState}, StopReturn1),
        ?_assertEqual({stop, normal, OldState}, TcpClosedReturn),
        ?_assertEqual({stop, normal, OldState}, ExitReturn),
        ?_assertEqual({stop, timeout, OldState}, TimeoutReturn),
        ?_assertEqual(<<"test">>, RespBinary),
        ?_assertEqual({stop, normal, OldState}, StopReturn2)
    ].