%%%-------------------------------------------------------------------
%%% @doc
%%% 协议处理模块
%%% @end
%%%-------------------------------------------------------------------
-module(ege_msg).

-include("ege_net.hrl").

%% API
-export([
    dispatch_msg/2,
    dispatch_msg/4,
    handle_return/3,
    dispatch_role_msg/4,
    handle_role_resp_maps/4,
    deal_error_cmd/2,
    decode/1,
    encode/2,
    decode_cmd/1,
    encode_cmd/2
]).

-define(ONE_MINUTE_SECONDS, 60).

%%====================================================================
%% Types
%%====================================================================

-export_type([
    cmd16/0, cmd/0, ccmd/0,
    err_lvl/0, msg/0, msg_type/0, request/0,
    state/0, handler_state/0, resp_maps/0
]).

-type cmd16() :: non_neg_integer().
-type cmd() :: non_neg_integer().
-type ccmd() :: non_neg_integer().
-type err_lvl() :: 0 | ?E_DEBUG | ?E_INFO | ?E_NOTICE | ?E_WARNING | ?E_ERROR | ?E_CRITICAL | ?E_ALERT | ?E_EMERGENCY.
-type msg_type() :: handle_pass.
-type request() :: binary() | {msg_type(), binary()}.
-type role() :: any().
-type handler_state() :: #net_handler{}.
-type state() :: handler_state() | role().
-type msg() :: tuple().
-type interval_config_key() :: term().
-type check_interval_limit_reply() :: true | pass | handle_pass | {IsPass :: boolean(), err_lvl()}.
-type resp_maps() :: #{
    stop := Reason :: any(), % close net_handler - optional
    state := state(), % new state - optional
    resp_bin_list := [binary()], % response to net_handler - optional
    error := any(), % error name - optional
    err_lvl:= err_lvl() % set err_lvl - optional
}.

-type stop_reply() :: {stop, Reason :: any(), handler_state()}.
-type stop_msg_reply() :: {stop, Reason :: any(), [binary()], handler_state()}.
-type dispatch_msg_reply() ::
    {ok, handler_state()} |
    {ok, [binary()], handler_state()} |
    stop_reply() | stop_msg_reply().

-callback(dispatch_msg(cmd16(), request(), state()) -> resp_maps()).
-callback(handle_pass(cmd16(), request(), state()) -> resp_maps() | skip).

%%====================================================================
%% API functions
%%====================================================================

%% @doc 协议处理
%% 1. 先根据 Cmd CCmd 判断是否超过间隔限制
%% 2. 再根据Cmd分发到msg处理模块
%% 3. msg处理根据CCmd调用具体的mod模块方法
-spec dispatch_msg(binary(), handler_state()) -> dispatch_msg_reply().
dispatch_msg(<<Cmd16:16, Binary/binary>>, State) ->
    case check_interval_limit(Cmd16, State#net_handler.interval_config_key) of
        true ->
            route_lv1(Cmd16, Binary, State, 0);
        {true, ErrLvl} ->
            route_lv1(Cmd16, Binary, State, ErrLvl);
        {false, ErrLvl} ->
            case ?MSG_ROUTER_MODULE:handle_pass(Cmd16, Binary, State) of
                skip ->
                    check_stop(ErrLvl, #{state => State}, {interval_limit, Cmd16});
                Maps ->
                    Error = maps:get(error, Maps, {interval_limit, Cmd16}),
                    NewErrLvl = maps:get(err_lvl, Maps, ErrLvl),
                    handle_resp_maps(Maps#{error => Error, err_lvl => NewErrLvl}, State, 0, Cmd16)
            end;
        handle_pass ->
            route_lv1(Cmd16, {handle_pass, Binary}, State, 0);
        pass ->
            case ?MSG_ROUTER_MODULE:handle_pass(Cmd16, Binary, State) of
                skip ->
                    {ok, State};
                Maps ->
                    handle_resp_maps(Maps, State, 0, Cmd16)
            end
    end;
dispatch_msg(Bin, State) ->
    {stop, {error_bin, Bin}, State}.

%% @doc 协议路由分发
%% MAX_NO_LOGIN_CMD = 最大未登录可以处理的协议号
%% 大于这个协议号的都会分发到角色进程处理
%% 小于等于这个协议号的协议在socket进程处理
-spec route_lv1(cmd16(), request(), handler_state(), err_lvl()) -> dispatch_msg_reply().
route_lv1(Cmd16, Request, State, ErrLvl) when Cmd16 =< ?MAX_NO_LOGIN_CMD ->
    dispatch_msg(Cmd16, Request, State, ErrLvl);
route_lv1(Cmd16, _Request, #net_handler{role_pid = undefined} = State, ErrLvl) ->
    check_stop(?E_WARNING + ErrLvl, #{state => State}, {not_login, Cmd16});
route_lv1(Cmd16, Request, #net_handler{role_pid = RolePid} = State, ErrLvl) ->
    RolePid ! {msg, Cmd16, Request},
    check_stop(ErrLvl, #{state => State}, {interval_limit, Cmd16}).

-spec dispatch_msg(module(), request(), handler_state(), err_lvl()) -> dispatch_msg_reply().
dispatch_msg(Cmd16, Binary, State, ErrLvl) ->
    case catch (?MSG_ROUTER_MODULE:dispatch_msg(Cmd16, Binary, State)) of
        {'EXIT', Reason} ->
            ?LOG_WARNING("error Cmd16:~p Reason:~p", [Cmd16, Reason]),
            dispatch_msg_exit(ErrLvl, State, Cmd16);
        Maps ->
            handle_resp_maps(Maps, State, ErrLvl, Cmd16)
    end.

-compile({inline, [handle_return/3]}).
-spec handle_return(cmd16(), err_lvl(), map()) -> dispatch_msg_reply().
handle_return(Cmd16, ErrLvl, #{error := Error} = Maps) ->
    case maps:get(err_lvl, Maps, ?E_NOTICE) of
        AddErrLvl when AddErrLvl >= ?E_ERROR ->
            ?LOG_DEBUG("dispatch_msg Cmd16:~p error:~p", [Cmd16, Error]),
            check_stop(AddErrLvl + ErrLvl, Maps, {Error, Cmd16});
        0 ->
            check_stop(ErrLvl, Maps, {interval_limit, Cmd16});
        AddErrLvl ->
            check_stop(AddErrLvl + ErrLvl, Maps, {Error, Cmd16})
    end;
handle_return(Cmd16, ErrLvl, Maps) ->
    check_stop(ErrLvl, Maps, {interval_limit, Cmd16}).


-spec dispatch_role_msg(cmd16(), request(), role(), pid()) -> {ok, role()} | {stop, role()}.
%% @doc 处理发往角色进程的协议
dispatch_role_msg(Cmd16, Binary, Role, NetHandlerPid) ->
    case catch ?MSG_ROUTER_MODULE:dispatch_msg(Cmd16, Binary, Role) of
        {'EXIT', Reason} ->
            ?LOG_WARNING("error Cmd16:~p Reason:~p", [Cmd16, Reason]),
            NetHandlerPid ! {err_lvl_up, ?E_ERROR, {handle_role_msg_exit, Cmd16}},
            {ok, Role};
        Maps ->
            handle_role_resp_maps(Maps, Role, NetHandlerPid, Cmd16)
    end.

-spec handle_role_resp_maps(map(), role(), pid(), cmd16()) -> {ok, role()} | {stop, role()}.
handle_role_resp_maps(#{stop := Reason} = Maps, Role0, NetHandlerPid, _Cmd16) ->
    ?LOG_DEBUG("net_handler stop err:~p", [Reason]),
    unlink(NetHandlerPid),
    case maps:find(resp_bin_list, Maps) of
        {ok, RespBinList} ->
            ege_net_handler:stop(NetHandlerPid, RespBinList);
        _ ->
            ege_net_handler:stop(NetHandlerPid)
    end,
    case maps:find(state, Maps) of
        {ok, Role} ->
            {stop, Role};
        _ ->
            {stop, Role0}
    end;
handle_role_resp_maps(Maps, Role0, NetHandlerPid, Cmd16) ->
    handle_error_maps(Maps, NetHandlerPid, Cmd16),
    case maps:find(resp_bin_list, Maps) of
        {ok, RespBinList} ->
            ege_net_handler:send(NetHandlerPid, RespBinList);
        _ -> ok
    end,
    case maps:find(state, Maps) of
        {ok, Role} ->
            {ok, Role};
        _ ->
            {ok, Role0}
    end.

%% @doc 处理错误 一分钟内不超过 30
-spec deal_error_cmd(err_lvl(), handler_state()) -> {ok, handler_state()} | stop.
deal_error_cmd(ErrLvl, _State) when ErrLvl >= ?MAX_ERR_LVL ->
    stop;
deal_error_cmd(ErrLvl, State) ->
    NowTime = erlang:system_time(second),
    deal_error_cmd(NowTime, State, ErrLvl).
-compile({inline, [deal_error_cmd/3]}).
deal_error_cmd(NowTime, #net_handler{last_cmd_time = LastCmdTime} = State, ErrLvl)
    when NowTime - LastCmdTime >= ?ONE_MINUTE_SECONDS -> %% 离上次间隔超过一分钟
    {ok, State#net_handler{err_times = ErrLvl, last_cmd_time = NowTime}};
deal_error_cmd(_NowTime, #net_handler{err_times = ErrTimes}, ErrLvl) when ErrTimes + ErrLvl >= ?MAX_ERR_LVL ->
%%    ?LOG_DEBUG("ErrTimes:~p", [ErrTimes]),
    stop;
deal_error_cmd(_NowTime, #net_handler{err_times = ErrTimes} = State, ErrLvl) ->
    {ok, State#net_handler{err_times = ErrTimes + ErrLvl}}.

%% @doc 对binary进行解码
-spec decode(binary()) -> {cmd16(), binary()}.
decode(<<Cmd16:16, Binary/binary>>) ->
    {Cmd16, Binary}.

%% @doc 编码
-spec encode(cmd16(), binary()) -> binary().
encode(Cmd16, Binary) ->
    <<Cmd16:16, Binary/binary>>.

-spec decode_cmd(cmd16()) -> {cmd(), ccmd()}.
decode_cmd(Cmd16) ->
    {Cmd16 bsr 9, Cmd16 band 2#111111111}.

-spec encode_cmd(cmd(), ccmd()) -> cmd16().
encode_cmd(Cmd, CCmd) ->
    Cmd bsl 9 + CCmd.

%%====================================================================
%% Internal functions
%%====================================================================

-compile({inline, [
    check_interval_limit/2,
    check_interval_limit_do/4
]}).

-spec check_stop(err_lvl(), map(), any()) -> dispatch_msg_reply().
check_stop(0, #{state := State, resp_bin_list := RespBinList}, _) ->
    {ok, RespBinList, State};
check_stop(0, #{state := State}, _) ->
    {ok, State};
check_stop(ErrLvl, #{state := State, resp_bin_list := RespBinList}, Reason) ->
    case deal_error_cmd(ErrLvl, State) of
        {ok, NewState} ->
            {ok, RespBinList, NewState};
        stop ->
            {stop, {error_cmd, Reason}, RespBinList, State}
    end;
check_stop(ErrLvl, #{state := State}, Reason) ->
    case deal_error_cmd(ErrLvl, State) of
        {ok, NewState} ->
            {ok, NewState};
        stop ->
            {stop, {error_cmd, Reason}, State}
    end.

%% @doc 检查发送协议的间隔是否符合配置要求
-spec check_interval_limit(cmd16(), interval_config_key()) -> check_interval_limit_reply().
check_interval_limit(Cmd16, IntervalConfigKey) ->
    LastTime = get(Cmd16),
    NowTime = erlang:system_time(millisecond),
    put(Cmd16, NowTime),
    case LastTime of
        undefined -> true;
        0 -> true;
        _ ->
            check_interval_limit_do(Cmd16, LastTime, NowTime, IntervalConfigKey)
    end.

-spec check_interval_limit_do(cmd16(), LastTime :: pos_integer(), NowTime :: pos_integer(), interval_config_key()) ->
    check_interval_limit_reply().
check_interval_limit_do(Cmd16, LastTime, NowTime, IntervalConfigKey) ->
    case interval_config(Cmd16, persistent_term:get(IntervalConfigKey, ?DEFAULT_INTERVAL_CONFIG)) of
        {Interval, Do} ->
            case NowTime - LastTime >= Interval of
                true -> true;
                _ -> Do
            end;
        true -> true
    end.

interval_config(Cmd16, [{Cmd16, Value} | _Config]) ->
    Value;
interval_config(Cmd16, [{Cmd16S, Cmd16E, Value} | _Config]) when Cmd16S >= Cmd16 orelse Cmd16 =< Cmd16E ->
    Value;
interval_config(_Cmd16, [{default, Value} | _Config]) ->
    Value;
interval_config(Cmd16, [_ | Config]) ->
    interval_config(Cmd16, Config);
interval_config(_Cmd16, []) -> true.

-spec dispatch_msg_exit(err_lvl(), handler_state(), cmd16()) -> stop_reply().
dispatch_msg_exit(ErrLvl, State, Cmd16) ->
    case deal_error_cmd(?E_ERROR + ErrLvl, State) of
        {ok, NewState} ->
            {ok, NewState};
        stop ->
            {stop, {error_cmd, {dispatch_msg_exit, Cmd16}}, State}
    end.

-spec handle_resp_maps(resp_maps(), state(), err_lvl(), cmd16()) -> dispatch_msg_reply().
handle_resp_maps(#{stop := Reason, state := NewState, resp_bin_list := RespBinList}, _State, _ErrLvl, _Cmd16) ->
    {stop, Reason, RespBinList, NewState};
handle_resp_maps(#{stop := Reason, resp_bin_list := RespBinList}, State, _ErrLvl, _Cmd16) ->
    {stop, Reason, RespBinList, State};
handle_resp_maps(#{stop := Reason, state := NewState}, _State, _ErrLvl, _Cmd16) ->
    {stop, Reason, NewState};
handle_resp_maps(#{stop := Reason}, State, _ErrLvl, _Cmd16) ->
    {stop, Reason, State};
handle_resp_maps(#{state := _} = Maps, _State, ErrLvl, Cmd16) ->
    handle_return(Cmd16, ErrLvl, Maps);
handle_resp_maps(Maps, State, ErrLvl, Cmd16) ->
    handle_return(Cmd16, ErrLvl, Maps#{state => State}).

handle_error_maps(#{error := Error} = Maps, NetHandlerPid, Cmd16) ->
    case maps:get(err_lvl, Maps, ?E_NOTICE) of
        ErrLvl when ErrLvl >= ?E_ERROR ->
            ?LOG_DEBUG("dispatch_msg Cmd16:~p error:~p", [Cmd16, Error]),
            NetHandlerPid ! {err_lvl_up, ErrLvl, {Error, Cmd16}};
        0 -> ok;
        ErrLvl ->
            NetHandlerPid ! {err_lvl_up, ErrLvl, {Error, Cmd16}}
    end,
    ok;
handle_error_maps(_Maps, _NetHandlerPid, _Cmd16) -> ok.