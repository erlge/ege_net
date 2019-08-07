%%%-------------------------------------------------------------------
%%% @doc
%%% 本模块有两个主要功能:
%%% 1. 担当进程,定时检查所有的handler的心跳
%%% 2. 封装了handler的通用方法
%%% @end
%%%-------------------------------------------------------------------
-module(ege_net_handler).
-behaviour(gen_server).
-include("ege_net.hrl").

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
%% API
-export([
    start_link/0,
    send/2, send_msg/2,
    stop/1, stop/2,
    handler_init/1, handler_terminate/0,
    loop_check_handler/0,
    start_check/0, stop_check/0,
    ip_to_bin/1, bin_to_ip/1,
    change_msg_interval_cb/2
]).

-define(HANDLER_GROUP, {p, l, handler_group}).
-define(TAB, gproc).
-ifndef(NET_HANDLER_TICK_TIME).
-define(NET_HANDLER_TICK_TIME, 3000).
-endif.
-define(START_TIMER, erlang:start_timer(?NET_HANDLER_TICK_TIME, self(), tick_check)).
-record(state, {
    timer_ref :: undefined | reference()
}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @doc 发送消息到handler
-spec send(pid() | undefined, binary() | [binary(), ...]) -> ok | undefined.
send(undefined, _Binary) -> undefined;
send(_Pid, []) -> ok;
send(Pid, BinOrBinList) ->
    Pid ! {send, BinOrBinList},
    ok.

%% @doc 发送消息到handler
-spec send_msg(pid() | undefined, ege_msg:msg() | [ege_msg:msg()]) -> ok | undefined.
send_msg(undefined, _Msg) -> undefined;
send_msg(_Pid, []) -> ok;
send_msg(Pid, MsgList) when is_list(MsgList) ->
    BinaryList = [?MSG_ROUTER_MODULE:encode_resp_msg(Msg) || Msg <- MsgList],
    Pid ! {send, BinaryList},
    ok;
send_msg(Pid, Msg) ->
    Binary = ?MSG_ROUTER_MODULE:encode_resp_msg(Msg),
    Pid ! {send, Binary},
    ok.

%% @doc 关闭handler
-spec stop(pid() | undefined) -> ok | undefined.
stop(undefined) -> undefined;
stop(Pid) -> Pid ! stop, ok.

%% @doc 关闭handler
-spec stop(pid() | undefined, binary() | [binary()]) -> ok | undefined.
stop(undefined, _Binary) -> undefined;
stop(Pid, BinOrBinList) -> Pid ! {stop, BinOrBinList}, ok.

%% @doc handler 回调 初始化
%% 1. 进程堆栈设置
%% 2. 加入handler组
%% 3. 初始化心跳时间
-spec handler_init(ege_msg:net_handler()) -> ege_msg:net_handler().
handler_init(NetHandler) ->
    process_flag(trap_exit, true),
    custom_heap_size(?HANDLER_CUSTOM_HEAP_SIZE_SWITCH),
    gproc:reg(?HANDLER_GROUP),
    NetHandler#net_handler{last_heartbeat = erlang:system_time(millisecond)}.

custom_heap_size(true) ->
    process_flag(min_heap_size, ?HANDLER_MIN_HEAP_SIZE),
    process_flag(min_bin_vheap_size, ?HANDLER_MIN_BIN_VHEAP_SIZE);
custom_heap_size(_) -> ok.

%% @doc handler 回调 结束
handler_terminate() -> ok.

%% @doc 检查所有的handler的心跳
loop_check_handler() ->
    send_to_group(?HANDLER_GROUP, {check_tick, erlang:system_time(millisecond)}).

%% @doc 打开检查开关
start_check() ->
    gen_server:cast(?MODULE, ?FUNCTION_NAME).

%% @doc 关闭检查开关
stop_check() ->
    gen_server:cast(?MODULE, ?FUNCTION_NAME).

%% @doc 格式化ip
-spec ip_to_bin(inet:ip4_address()) -> binary().
ip_to_bin({A, B, C, D}) ->
    list_to_binary(lists:concat([A, ".", B, ".", C, ".", D])).

%% @doc 转换为ip
-spec bin_to_ip(Ip :: iolist()) -> inet:ip4_address().
bin_to_ip(IpStr) when is_binary(IpStr) ->
    inet:parse_address(IpStr);
bin_to_ip(IpBin) when is_binary(IpBin) ->
    inet:parse_address(binary_to_list(IpBin)).

change_msg_interval_cb(Pid, Fun) when is_function(Fun, 2) ->
    Pid ! {change_msg_interval_cb, Fun}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    {ok, #state{timer_ref = ?START_TIMER}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(start_check, #state{timer_ref = undefined} = State) -> % not start
    {noreply, State#state{timer_ref = ?START_TIMER}};
handle_cast(start_check, State) -> % started
    {noreply, State};
handle_cast(stop_check, #state{timer_ref = undefined} = State) -> % not start
    {noreply, State};
handle_cast(stop_check, State) -> % started
    erlang:cancel_timer(State#state.timer_ref),
    {noreply, State#state{timer_ref = undefined}};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({timeout, TimerRef, tick_check}, #state{timer_ref = TimerRef} = State) ->
    loop_check_handler(),
    {noreply, State#state{timer_ref = ?START_TIMER}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    cancel_timer(State#state.timer_ref),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

cancel_timer(undefined) -> ok;
cancel_timer(TimerRef) ->
    erlang:cancel_timer(TimerRef).

send_to_group(Key, Msg) ->
    lists:foreach(
        fun(Pid) ->
            Pid ! Msg
        end, ets:select(?TAB, [{{{Key, '_'}, '$1', '_'}, [], ['$1']}])).