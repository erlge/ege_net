%%%-------------------------------------------------------------------
%%% @doc
%%% web socket handler
%%% @end
%%%-------------------------------------------------------------------
-module(ege_ws_handler).
-behaviour(cowboy_websocket).

-include("ege_net.hrl").

-export([
    init/2,
    websocket_init/1,
    websocket_handle/2,
    websocket_info/2,
    terminate/3
]).

%%%===================================================================
%%% cowboy_websocket callbacks
%%%===================================================================

init(Req, Opts) ->
    IpBin =
        case cowboy_req:header(<<"x-forwarded-for">>, Req) of
            undefined ->
                get_ip_from_peer(Req);
            XRealIp ->
                case binary:split(XRealIp, [<<",">>, <<" ">>], [global, trim_all]) of
                    [FirstIp | _] -> FirstIp;
                    _ ->
                        get_ip_from_peer(Req)
                end
        end,
    {cowboy_websocket, Req, {IpBin, Opts}}.

websocket_init({IpBin, _Opts}) ->
    State = ege_net_handler:handler_init(#net_handler{ip = IpBin}),
    {ok, State}.

websocket_handle({binary, ReqBinary}, State) ->
    case ege_msg:dispatch_msg(ReqBinary, State) of
        {ok, RespBinList, NewState} ->
            send_resp(RespBinList, NewState);
        {ok, NewState} ->
            {ok, NewState};
        {stop, Reason, RespBinList, NewState} ->
            ?LOG_DEBUG("net_handler stop err:~p", [Reason]),
            send_stop_resp(RespBinList, NewState);
        {stop, Reason, NewState} ->
            ?LOG_DEBUG("net_handler stop err:~p", [Reason]),
            {stop, NewState}
    end;
websocket_handle({ping, Binary}, State) ->
    {reply, {pong, Binary}, State};
websocket_handle(ping, State) ->
    {reply, pong, State};
websocket_handle({text, _Binary}, State) ->
    {reply, {text, <<"nonsupport">>}, State};
websocket_handle(Data, State) ->
    ?LOG_WARNING("unknow msg:~p", [Data]),
    {ok, State}.

websocket_info({send, []}, State) -> {ok, State};
websocket_info({send, BinaryList}, State) when is_list(BinaryList) ->
    FrameList = [{binary, Binary} || Binary <- BinaryList],
    {reply, FrameList, State};
websocket_info({send, Binary}, State) ->
    {reply, {binary, Binary}, State};
websocket_info({check_tick, NowMs}, State) ->
    case ?CHECK_TICK(State, NowMs) of
        true ->
            ?LOG_DEBUG("net_handler stop err:~p", [?HEARTBEAT_TIMEOUT_REASON]),
            {stop, State};
        _ ->
            {ok, State}
    end;
websocket_info({err_lvl_up, ErrLvl, Reason}, State) ->
    case ege_msg:deal_error_cmd(ErrLvl, State) of
        {ok, NewState} ->
            {ok, NewState};
        stop ->
            ?LOG_DEBUG("net_handler net_handler stop err:~p", [Reason]),
            {stop, State}
    end;
websocket_info({stop, BinaryList}, State) when is_list(BinaryList) ->
    send_stop_resp(BinaryList, State);
websocket_info({stop, Binary}, State) ->
    {reply, [{binary, Binary}, {close, 1000, <<>>}], State};
websocket_info(stop, State) ->
    {stop, State};
websocket_info({'EXIT', _, _Reason}, State) ->
    {stop, State};
websocket_info({change_msg_interval_cb, Fun}, State) when is_function(Fun, 2) ->
    {ok, State#net_handler{interval_config_key = Fun}};
websocket_info(Info, State) ->
    ?LOG_WARNING("unknow info:~p", [Info]),
    {ok, State}.

terminate(_Reason, _Req, _State) ->
    ege_net_handler:handler_terminate(),
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-compile({inline, [
    send_stop_resp/2,
    send_resp/2,
    get_ip_from_peer/1
]}).

send_stop_resp(RespBinList, State) ->
    FrameList = [{binary, Binary} || Binary <- RespBinList],
    {reply, FrameList ++ [{close, 1000, <<>>}], State}.

send_resp([], State) -> {ok, State};
send_resp(RespBinList, State) ->
    FrameList = [{binary, Binary} || Binary <- RespBinList],
    {reply, FrameList, State}.

get_ip_from_peer(Req) ->
    {Ip, _Port} = cowboy_req:peer(Req),
    ege_net_handler:ip_to_bin(Ip).