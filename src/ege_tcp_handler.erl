%%%-------------------------------------------------------------------
%%% @doc
%%% tcp handler
%%% @end
%%%-------------------------------------------------------------------
-module(ege_tcp_handler).

-behaviour(ranch_protocol).
-behaviour(gen_server).

-include("ege_msg.hrl").
-include("ege_net.hrl").

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/4
]).

-define(SET_ACTIVE_ONCE(Socket), inet:setopts(Socket, [{active, once}])).

-record(state, {
    socket :: inet:socket(),
    sub_state :: ege_msg:handler_state()
}).
-type opts() :: proplists:proplist().

%%%===================================================================
%%% ranch_protocol callbacks
%%%===================================================================

-spec start_link(ranch:ref(), inet:socket(), module(), opts()) -> {ok, pid()}.
start_link(Ref, Socket, Transport, Opts) ->
    {ok, proc_lib:spawn_link(?MODULE, init, [{Ref, Socket, Transport, Opts}])}.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({Ref, Socket, ranch_tcp, Opts}) ->
    ok = ranch:accept_ack(Ref),

    Packet = proplists:get_value(packet, Opts, 2),
    ok = ranch_tcp:setopts(Socket, [{active, once}, {packet, Packet}]),

    {ok, {Ip, _Port}} = ranch_tcp:peername(Socket),
    IpBin = ege_net_handler:ip_to_bin(Ip),
    SubState = ege_net_handler:handler_init(#net_handler{ip = IpBin}),

    gen_server:enter_loop(?MODULE, [], #state{sub_state = SubState, socket = Socket}, ?TCP_TIMEOUT_SHUTDOWN).

handle_info({tcp, Socket, ReqBinary}, State) ->
    OldSubState = State#state.sub_state,
    case ege_msg:dispatch_msg(ReqBinary, OldSubState) of
        {ok, RespBinList, SubState} ->
            send(RespBinList, Socket),
            ?SET_ACTIVE_ONCE(Socket),
            {noreply, State#state{sub_state = SubState}};
        {ok, SubState} ->
            ?SET_ACTIVE_ONCE(Socket),
            {noreply, State#state{sub_state = SubState}};
        {stop, Reason, RespBinList, SubState} ->
            ?LOG_DEBUG("net_handler stop err:~p", [Reason]),
            send(RespBinList, Socket),
            ?SET_ACTIVE_ONCE(Socket),
            {stop, normal, State#state{sub_state = SubState}};
        {stop, Reason, SubState} ->
            ?LOG_DEBUG("net_handler stop err:~p", [Reason]),
            ?SET_ACTIVE_ONCE(Socket),
            {stop, normal, State#state{sub_state = SubState}}
    end;
handle_info({send, BinOrBinList}, State) ->
    send(BinOrBinList, State#state.socket),
    {noreply, State};
handle_info({check_tick, NowMs}, State) ->
    case ?CHECK_TICK(State#state.sub_state, NowMs) of
        true ->
            ?LOG_DEBUG("net_handler stop err:~p", [?HEARTBEAT_TIMEOUT_REASON]),
            {stop, normal, State};
        _ ->
            {noreply, State}
    end;
handle_info({err_lvl_up, ErrLvl, Reason}, State) ->
    case ege_msg:deal_error_cmd(ErrLvl, State#state.sub_state) of
        {ok, SubState} ->
            {noreply, State#state{sub_state = SubState}};
        stop ->
            ?LOG_DEBUG("net_handler stop err:~p", [Reason]),
            {stop, normal, State}
    end;
handle_info({tcp_closed, _Socket}, State) ->
    {stop, normal, State};
handle_info(timeout, State) -> %% timeout_stop
    {stop, timeout, State};
handle_info({stop, BinOrBinList}, State) ->
    send(BinOrBinList, State#state.socket),
    {stop, normal, State};
handle_info(stop, State) ->
    {stop, normal, State};
handle_info({'EXIT', _, _Reason}, State) ->
    {stop, normal, State};
handle_info({change_msg_interval_cb, Fun}, State) when is_function(Fun, 2) ->
    SubState = State#state.sub_state,
    {noreply, State#state{sub_state = SubState#net_handler{interval_config_key = Fun}}};
handle_info(Info, State) ->
    ?LOG_WARNING("unknow info:~p", [Info]),
    {noreply, State}.

handle_call(Request, _From, State) ->
    ?LOG_WARNING("unknow call:~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?LOG_WARNING("unknow cast:~p", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ege_net_handler:handler_terminate(),
    ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send(BinaryList, Socket) when is_list(BinaryList) ->
    lists:foreach(
        fun(Binary) ->
            gen_tcp:send(Socket, Binary)
        end, BinaryList);
send(Binary, Socket) ->
    gen_tcp:send(Socket, Binary).