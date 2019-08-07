%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ege_msg_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ege_net.hrl").
-include("msg.hrl").
-include("msg_base.hrl").
-include("msg_base_pb.hrl").

-record(role, {role_id, n_handler_pid}).

-define(ONE_MINUTE_SECONDS, 60).

x_code_cmd_test() ->
    Cmd16 = 513,
    ?assertEqual({Cmd16, <<"test">>}, ege_msg:decode(<<Cmd16:16, "test">>)),
    ?assertEqual(<<Cmd16:16, "test">>, ege_msg:encode(Cmd16, <<"test">>)),

    ?assertEqual({1, 1}, ege_msg:decode_cmd(Cmd16)),
    ?assertEqual(Cmd16, ege_msg:encode_cmd(1, 1)),
    ok.

dispatch_msg_3_test() ->
    meck:new(msg_router, [non_strict]),
    meck:expect(msg_router, handle_pass, 3, skip),

    Now = erlang:system_time(second),
    Cmd16 = 513,

    State = #net_handler{last_cmd_time = Now - 1}, %% 设置为当前时间减一秒,不大于一分钟不会保存新时间
    NewState = #net_handler{last_cmd_time = Now - 1, last_heartbeat = Now},
    ege_msg_interval:set_config([#{do => true}]),

    %% error maps
    ErrMaps1 = #{error => test_error, err_lvl => ?E_DEBUG},
    meck:expect(msg_router, dispatch_msg, 3, ErrMaps1),
    ?assertEqual({ok, State#net_handler{err_times = ?E_DEBUG}},
        ege_msg:dispatch_msg(<<Cmd16:16>>, State)),

    ErrMaps2 = #{error => test_error, err_lvl => 0},
    meck:expect(msg_router, dispatch_msg, 3, ErrMaps2),
    ?assertEqual({ok, State}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),

    ErrMaps3 = #{error => test_error, err_lvl => ?E_CRITICAL, state => State},
    meck:expect(msg_router, dispatch_msg, 3, ErrMaps3),
    ?assertEqual({stop, {error_cmd, {test_error, Cmd16}}, State},
        ege_msg:dispatch_msg(<<Cmd16:16>>, State)),

    %% MAX_NO_LOGIN_CMD
    meck:expect(msg_router, dispatch_msg, 3, {'EXIT', test_error}),
    ?assertEqual({ok, State#net_handler{err_times = ?E_WARNING}},
        ege_msg:dispatch_msg(<<(?MAX_NO_LOGIN_CMD + 1):16>>, State)),
    Pid = self(),
    ?assertEqual({ok, State#net_handler{role_pid = Pid}},
        ege_msg:dispatch_msg(<<(?MAX_NO_LOGIN_CMD + 1):16>>, State#net_handler{role_pid = Pid})),
    receive
        Msg -> Msg
    end,
    ?assertEqual({msg, ?MAX_NO_LOGIN_CMD + 1, <<>>}, Msg),

    %% true
    meck:expect(msg_router, dispatch_msg, 3, {'EXIT', test_error}),
    ?assertEqual({ok, State#net_handler{err_times = ?E_ERROR}},
        ege_msg:dispatch_msg(<<Cmd16:16>>, State)),
    StopErrorState = State#net_handler{err_times = ?E_ERROR},
    ?assertEqual({stop, {error_cmd, {dispatch_msg_exit, Cmd16}}, StopErrorState},
        ege_msg:dispatch_msg(<<Cmd16:16>>, StopErrorState)),

    meck:expect(msg_router, dispatch_msg, 3, #{stop => test_stop}),
    ?assertEqual({stop, test_stop, State}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),

    %% {true, ErrLvl}
    meck:expect(msg_router, dispatch_msg, 3, #{}),
    ege_msg_interval:set_config([
        #{do => debug, interval => 100, is_pass => true}
    ]),
    ?assertEqual({ok, State#net_handler{err_times = ?E_DEBUG}},
        ege_msg:dispatch_msg(<<Cmd16:16>>, State)),

    %% {false, ErrLvl}
    ege_msg_interval:set_config([
        #{do => debug, interval => 100}
    ]),
    ?assertEqual({ok, State#net_handler{err_times = ?E_DEBUG}},
        ege_msg:dispatch_msg(<<Cmd16:16>>, State)),

    %% handle_pass
    ege_msg_interval:set_config([
        #{do => handle_pass, interval => 100}
    ]),
    meck:expect(msg_router, dispatch_msg, 3, #{}),
    ?assertEqual({ok, State}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),
    meck:expect(msg_router, dispatch_msg, 3, {'EXIT', test_error}),
    ?assertEqual({ok, State#net_handler{err_times = ?E_ERROR}},
        ege_msg:dispatch_msg(<<Cmd16:16>>, State)),
    meck:expect(msg_router, dispatch_msg, 3, #{stop => test_stop}),
    ?assertEqual({stop, test_stop, State}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),
    meck:expect(msg_router, dispatch_msg, 3, #{stop => test_stop, resp_bin_list => [<<>>]}),
    ?assertEqual({stop, test_stop, [<<>>], State}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),
    meck:expect(msg_router, dispatch_msg, 3, #{stop => test_stop, state => NewState}),
    ?assertEqual({stop, test_stop, NewState}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),
    meck:expect(msg_router, dispatch_msg, 3, #{stop => test_stop, state => NewState, resp_bin_list => [<<>>]}),
    ?assertEqual({stop, test_stop, [<<>>], NewState}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),
    meck:expect(msg_router, dispatch_msg, 3, #{stop => test_stop}),
    ?assertEqual({stop, test_stop, State}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),
    meck:expect(msg_router, dispatch_msg, 3, #{state => State}),
    ?assertEqual({ok, State}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),

    %% pass
    ege_msg_interval:set_config([
        #{do => pass, interval => 100}
    ]),
    ?assertEqual({ok, State}, ege_msg:dispatch_msg(<<Cmd16:16>>, State)),

    meck:unload(msg_router),
    ok.

dispatch_msg_error_test() ->
    State = #net_handler{},
    ?assertEqual({stop, {error_bin, <<>>}, State}, ege_msg:dispatch_msg(<<>>, State)),
    ?assertEqual({stop, {error_bin, <<1>>}, State}, ege_msg:dispatch_msg(<<1>>, State)),
    ok.

handle_role_msg_test() ->
    meck:new(msg_router, [non_strict]),
    meck:expect(msg_router, handle_pass, 3, skip),

    Role = #role{},
    NetHandlerPid = self(),
    Cmd16 = 513,

    %% exit
    meck:expect(msg_router, dispatch_msg, 3, {'EXIT', test_error}),
    ?assertEqual({ok, Role}, ege_msg:dispatch_role_msg(Cmd16, <<>>, Role, NetHandlerPid)),
    receive
        {err_lvl_up, _, _} = IsStop1 -> IsStop1
    end,
    ?assertEqual({err_lvl_up, ?E_ERROR, {handle_role_msg_exit, Cmd16}}, IsStop1),

    meck:expect(msg_router, dispatch_msg, 4, {'EXIT', test_error}),
    ?assertEqual({ok, Role}, ege_msg:dispatch_role_msg(Cmd16, {handle_pass, <<>>}, Role, NetHandlerPid)),
    receive
        {err_lvl_up, _, _} = IsStop2 -> IsStop2
    end,
    ?assertEqual({err_lvl_up, ?E_ERROR, {handle_role_msg_exit, Cmd16}}, IsStop2),

    %% stop
    meck:expect(msg_router, dispatch_msg, 3, #{stop => test_stop}),
    ?assertEqual({stop, Role}, ege_msg:dispatch_role_msg(Cmd16, <<>>, Role, NetHandlerPid)),
    receive
        StopMsg1 -> StopMsg1
    end,
    ?assertEqual(stop, StopMsg1),
    meck:expect(msg_router, dispatch_msg, 3, #{stop => test_stop, resp_bin_list => [<<"stop">>]}),
    ?assertEqual({stop, Role}, ege_msg:dispatch_role_msg(Cmd16, <<>>, Role, NetHandlerPid)),
    receive
        StopMsg2 -> StopMsg2
    end,
    ?assertEqual({stop, [<<"stop">>]}, StopMsg2),
    Role1 = Role#role{role_id = 1},
    meck:expect(msg_router, dispatch_msg, 3, #{stop => test_stop, state => Role1}),
    ?assertEqual({stop, Role1}, ege_msg:dispatch_role_msg(Cmd16, <<>>, Role, NetHandlerPid)),
    receive
        StopMsg3 -> StopMsg3
    end,
    ?assertEqual(stop, StopMsg3),

    %% error
    meck:expect(msg_router, dispatch_msg, 3, #{error => test_stop}),
    ?assertEqual({ok, Role}, ege_msg:dispatch_role_msg(Cmd16, <<>>, Role, NetHandlerPid)),
    receive
        ErrorMsg1 -> ErrorMsg1
    end,
    ?assertEqual({err_lvl_up, ?E_NOTICE, {test_stop, Cmd16}}, ErrorMsg1),

    meck:expect(msg_router, dispatch_msg, 3, #{error => test_stop, err_lvl => 0}),
    ?assertEqual({ok, Role}, ege_msg:dispatch_role_msg(Cmd16, <<>>, Role, NetHandlerPid)),

    meck:expect(msg_router, dispatch_msg, 3, #{error => test_stop, err_lvl => ?E_ERROR}),
    ?assertEqual({ok, Role}, ege_msg:dispatch_role_msg(Cmd16, <<>>, Role, NetHandlerPid)),
    receive
        ErrorMsg2 -> ErrorMsg2
    end,
    ?assertEqual({err_lvl_up, ?E_ERROR, {test_stop, Cmd16}}, ErrorMsg2),

    %% normal
    meck:expect(msg_router, dispatch_msg, 3, #{state => Role1}),
    ?assertEqual({ok, Role1}, ege_msg:dispatch_role_msg(Cmd16, <<>>, Role, NetHandlerPid)),
    meck:expect(msg_router, dispatch_msg, 3, #{resp_bin_list => [<<"normal">>]}),
    ?assertEqual({ok, Role}, ege_msg:dispatch_role_msg(Cmd16, <<>>, Role, NetHandlerPid)),
    receive
        NormalMsg1 -> NormalMsg1
    end,
    ?assertEqual({send, [<<"normal">>]}, NormalMsg1),
    meck:expect(msg_router, dispatch_msg, 4, #{resp_bin_list => [<<"normal">>]}),
    ?assertEqual({ok, Role}, ege_msg:dispatch_role_msg(Cmd16, {handle_pass, <<>>}, Role, NetHandlerPid)),
    receive
        NormalMsg2 -> NormalMsg2
    end,
    ?assertEqual({send, [<<"normal">>]}, NormalMsg2),

    meck:unload(msg_router),
    ok.

dispatch_msg_2_test() ->
    ege_msg_interval:set_config([#{do => true}]),
    Now = erlang:system_time(second),
    Binary = ?MSG_ROUTER_MODULE:encode_req_msg(#heartbeat_req{id = 1}),
    Resp = ?MSG_ROUTER_MODULE:encode_resp_msg(#heartbeat_resp{id = 1}),
    State = #net_handler{last_heartbeat = Now},
    {ok, [Resp], NewState} = ege_msg:dispatch_msg(Binary, State),
    ?assertEqual(Now, NewState#net_handler.last_heartbeat - 1),
    ok.

check_stop_test() ->
    State = #net_handler{last_cmd_time = erlang:system_time(second) - 1},

    ?assertEqual({ok, State}, ege_msg:check_stop(0, #{state => State}, 0)),
    ?assertEqual({ok, [<<>>], State}, ege_msg:check_stop(0, #{state => State, resp_bin_list => [<<>>]}, 0)),

    ?assertEqual({stop, {error_cmd, test_error}, State},
        ege_msg:check_stop(?MAX_ERR_LVL, #{state => State}, test_error)),
    ?assertEqual({stop, {error_cmd, test_error}, [<<>>], State},
        ege_msg:check_stop(?MAX_ERR_LVL, #{state => State, resp_bin_list => [<<>>]}, test_error)),

    ?assertEqual({ok, State#net_handler{err_times = ?E_DEBUG}},
        ege_msg:check_stop(?E_DEBUG, #{state => State}, test_error)),
    ?assertEqual({ok, [<<>>], State#net_handler{err_times = ?E_DEBUG}},
        ege_msg:check_stop(?E_DEBUG, #{state => State, resp_bin_list => [<<>>]}, test_error)),
    ok.

deal_error_cmd_test() ->
    State = #net_handler{last_cmd_time = erlang:system_time(second) - 1},
    %% 最大值
    ?assertEqual(stop, ege_msg:deal_error_cmd(?MAX_ERR_LVL, State)),
    ?assertEqual(stop, ege_msg:deal_error_cmd(?E_CRITICAL, State)),

    ?assertEqual({ok, State#net_handler{err_times = ?E_DEBUG}},
        ege_msg:deal_error_cmd(?E_DEBUG, State)),
    ?assertEqual({ok, State#net_handler{err_times = ?E_DEBUG + ?E_INFO}},
        ege_msg:deal_error_cmd(?E_INFO, State#net_handler{err_times = ?E_DEBUG})),
    ?assertEqual(stop, ege_msg:deal_error_cmd(?E_ERROR, State#net_handler{err_times = ?E_ERROR})),

    %% 小于 ONE_MINUTE_SECONDS
    ?assertEqual(stop, ege_msg:deal_error_cmd(1, #net_handler{err_times = ?E_ERROR}, ?E_ERROR)), %% 16+16=32

    %% 等于 ONE_MINUTE_SECONDS
    ?assertEqual({ok, #net_handler{err_times = ?E_ERROR, last_cmd_time = ?ONE_MINUTE_SECONDS}},
        ege_msg:deal_error_cmd(?ONE_MINUTE_SECONDS, #net_handler{err_times = ?E_ERROR}, ?E_ERROR)),

    %% 大于 ONE_MINUTE_SECONDS
    ?assertEqual({ok, #net_handler{err_times = ?E_ERROR, last_cmd_time = ?ONE_MINUTE_SECONDS + 1}},
        ege_msg:deal_error_cmd(1 + ?ONE_MINUTE_SECONDS, #net_handler{err_times = ?E_ERROR}, ?E_ERROR)),
    ok.

check_interval_limit_test() ->
    erlang:erase(),
    Cmd16 = 513,

    %% no LastTime
    ?assertEqual(true, check_interval_limit(Cmd16)),

    %% LastTime=0
    set_last_time(1, 1, 0),
    ?assertEqual(true, check_interval_limit(Cmd16)),

    %% LastTime=/=0 & no_limit
    ege_msg_interval:set_config([#{do => true}]),
    ?assertEqual(true, check_interval_limit(Cmd16)),

    %% Interval=100ms
    ege_msg_interval:set_config([#{do => debug, interval => 100}]),
    ?assertEqual({false, ?EGEN_LEVEL2NUM(debug)}, check_interval_limit_do(Cmd16, 1, 2)),
    ?assertEqual(true, check_interval_limit_do(Cmd16, 1, 101)),

    %% Interval=100ms & is_pass=true
    ege_msg_interval:set_config([#{do => debug, is_pass => true, interval => 100}]),
    ?assertEqual({true, ?EGEN_LEVEL2NUM(debug)}, check_interval_limit_do(Cmd16, 1, 2)),
    ?assertEqual(true, check_interval_limit_do(Cmd16, 1, 101)),

    %% Interval=100ms & pass
    ege_msg_interval:set_config([#{do => pass, interval => 100}]),
    ?assertEqual(pass, check_interval_limit_do(Cmd16, 1, 2)),
    ?assertEqual(true, check_interval_limit_do(Cmd16, 1, 101)),

    %% Interval=100ms & handle_pass
    ege_msg_interval:set_config([#{do => handle_pass, interval => 100}]),
    ?assertEqual(handle_pass, check_interval_limit_do(Cmd16, 1, 2)),
    ?assertEqual(true, check_interval_limit_do(Cmd16, 1, 101)),
    ok.

set_last_time(Cmd, CCmd, LastTime) ->
    Index = Cmd bsl 7 + CCmd,
    put(Index, LastTime).

check_interval_limit(Cmd16) ->
    ege_msg:check_interval_limit(Cmd16, msg_interval_config).

check_interval_limit_do(Cmd16, LastTime, NowTime) ->
    ege_msg:check_interval_limit_do(Cmd16, LastTime, NowTime, msg_interval_config).