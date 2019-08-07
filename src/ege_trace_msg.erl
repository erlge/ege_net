%%%-------------------------------------------------------------------
%%% @doc
%%% 协议调试方法封装
%%% @end
%%%-------------------------------------------------------------------
-module(ege_trace_msg).
-include_lib("stdlib/include/ms_transform.hrl").
-include("ege_net.hrl").

%% API
-export([
    trace_msg/0, trace_msg/1, trace_msg/2, trace_msg/3,
    close_trace/0,
    get_tspecs/3, msg_format/1
]).

%% 定义心跳包协议为 请求:{0,0}/响应:{0,1}
-ifdef(CMD_HEARTBEAT_REQ).
-define(CMD_MSG_BASE_HEARTBEAT_REQ, ?CMD_HEARTBEAT_REQ). %% heartbeat_req
-else.
-define(CMD_MSG_BASE_HEARTBEAT_REQ, 0). %% heartbeat_req
-endif.
-ifdef(CMD_HEARTBEAT_RESP).
-define(CMD_MSG_BASE_HEARTBEAT_RESP, ?CMD_HEARTBEAT_RESP). %% heartbeat_resp
-else.
-define(CMD_MSG_BASE_HEARTBEAT_RESP, 1). %% heartbeat_resp
-endif.

%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc 监听全部消息
trace_msg() ->
    trace_msg('_', '_', []).

%% @doc 监听消息
%% 1. 根据配置内容监听全部消息
%% 2. 只监听Cmd匹配的消息
trace_msg(Config) when is_list(Config) ->
    trace_msg('_', '_', Config);
trace_msg(Cmd) ->
    trace_msg(Cmd, '_', []).

%% @doc 监听消息
%% 1. 根据配置内容只监听 Cmd 匹配的消息
%% 2. 只监听 Cmd 和 CCmd 都匹配的消息
trace_msg(Cmd, Config) when is_list(Config) ->
    trace_msg(Cmd, '_', Config);
trace_msg(Cmd, CCmd) ->
    trace_msg(Cmd, CCmd, []).

%% @doc 监听消息
%% 根据配置内容只监听 Cmd 和 CCmd 都匹配的消息
trace_msg(Cmd, CCmd, Config0) when is_list(Config0) ->
    {TraceType, Config1} =
        case lists:keytake(trace_type, 1, Config0) of
            {value, {_, TraceTypeT}, ConfigT} ->
                {TraceTypeT, ConfigT};
            _ ->
                {all, Config0}
        end,
    Config2 =
        case proplists:get_value(to_file, Config1) of
            undefined -> Config1;
            true ->
                {ok, Dev} = file:open("trace_msg.log", [write]),
                io:format("trace_file Dev:~p~n", [Dev]),
                put(trace_file, Dev),
                [{io_server, Dev} | Config1];
            Filename ->
                {ok, Dev} = file:open(Filename, [write]),
                io:format("trace_file Dev:~p~n", [Dev]),
                put(trace_file, Dev),
                [{io_server, Dev} | Config1]
        end,
    Config =
        case proplists:is_defined(enable_formatter, Config0) of
            true ->
                [{formatter, fun ?MODULE:msg_format/1} | Config2];
            _ -> Config2
        end,
    TraceCount = proplists:get_value(trace_count, Config0, 200),
    TSpecs = get_tspecs(Cmd, CCmd, TraceType),
    recon_trace:calls(TSpecs, TraceCount, [{scope, local} | Config]).

close_trace() ->
    case erlang:erase(trace_file) of
        undefined -> ignore;
        Dev ->
            file:close(Dev)
    end,
    recon_trace:clear().

get_tspecs('_', '_', TraceType) ->
    get_tspecs_do(TraceType, '_', '_');
get_tspecs('_', CCmdName, _TraceType) when is_atom(CCmdName) ->
    error(must_define_cmd);
get_tspecs(CmdName, '_', TraceType) when is_atom(CmdName) ->
    Cmd = ?MSG_ROUTER_MODULE:msg_cmd(CmdName),
    get_tspecs_do(TraceType, Cmd, '_');
get_tspecs(Cmd, '_', TraceType) when is_integer(Cmd) ->
    get_tspecs_do(TraceType, Cmd, '_');
get_tspecs(CmdName, CCmdName, TraceType) when is_atom(CmdName) andalso is_atom(CCmdName) ->
    Cmd16 = ?MSG_ROUTER_MODULE:msg_cmd16(CCmdName),
    {Cmd, CCmd} = ege_msg:decode_cmd(Cmd16),
    get_tspecs_do(TraceType, Cmd, CCmd);
get_tspecs(Cmd, CCmdName, TraceType) when is_integer(Cmd) andalso is_atom(CCmdName) ->
    CCmd = ?MSG_ROUTER_MODULE:msg_ccmd(CCmdName),
    get_tspecs_do(TraceType, Cmd, CCmd);
get_tspecs(CmdName, CCmd, TraceType) when is_atom(CmdName) andalso is_integer(CCmd) ->
    Cmd = ?MSG_ROUTER_MODULE:msg_cmd(CmdName),
    get_tspecs_do(TraceType, Cmd, CCmd);
get_tspecs(Cmd, CCmd, TraceType) when is_integer(Cmd) andalso is_integer(CCmd) ->
    get_tspecs_do(TraceType, Cmd, CCmd).

msg_format(TraceMsg) ->
    {Type, Pid, {Hour, Min, Sec}, TraceInfo} = extract_info(TraceMsg),
    case msg_format(Type, TraceInfo) of
        ignore -> [];
        {FormatStr, FormatArgs} ->
            io_lib:format("~n~p:~p:~9.6.0f ~p " ++ FormatStr ++ "~n", [Hour, Min, Sec, Pid] ++ FormatArgs)
    end.
%% {trace, Pid, call, {M, F, Args}}
msg_format(call, [{ege_msg, route_lv1, [?CMD_MSG_BASE_HEARTBEAT_REQ, _Binary, _State, _ErrLvl]}]) ->
    ignore;
msg_format(call, [{ege_msg, route_lv1, [Cmd16, {handle_pass, Binary}, State, _ErrLvl]}]) ->
    Input = ?MSG_ROUTER_MODULE:decode_req_msg(<<Cmd16:16, Binary/binary>>),
    {"[receive] Cmd:~p Req:~s State:~s", [Cmd16, recon_trace:format_trace_output(Input), recon_trace:format_trace_output(State)]};
msg_format(call, [{ege_msg, route_lv1, [Cmd16, Binary, State, _ErrLvl]}]) ->
    Input = ?MSG_ROUTER_MODULE:decode_req_msg(<<Cmd16:16, Binary/binary>>),
    {"[receive] Cmd:~p Req:~s State:~s", [Cmd16, recon_trace:format_trace_output(Input), recon_trace:format_trace_output(State)]};
msg_format(call, [{cowboy_websocket, websocket_send, [RespBinaryOrRespBinaryList, _]}]) ->
    decode_send(RespBinaryOrRespBinaryList);
msg_format(call, [{ege_tcp_handler, send, [RespBinaryOrRespBinaryList, _]}]) ->
    decode_send(RespBinaryOrRespBinaryList);
%% {trace, Pid, return_from, {M, F, Arity}, ReturnValue}
msg_format(return_from, [{ege_msg, route_lv1, 4}, Return]) ->
    {"[return] ege_msg:route_lv1/4 --> ~s", [recon_trace:format_trace_output(Return)]};
msg_format(_Type, _TraceInfo) ->
    ignore.

%%%===================================================================
%%% Internal functions
%%%===================================================================

extract_info(TraceMsg) ->
    case tuple_to_list(TraceMsg) of
        [trace_ts, Pid, Type | Info] ->
            {TraceInfo, [Timestamp]} = lists:split(length(Info) - 1, Info),
            {Type, Pid, to_hms(Timestamp), TraceInfo};
        [trace, Pid, Type | TraceInfo] ->
            {Type, Pid, to_hms(os:timestamp()), TraceInfo}
    end.

to_hms(Stamp = {_, _, Micro}) ->
    {_, {H, M, Secs}} = calendar:now_to_local_time(Stamp),
    Seconds = Secs rem 60 + (Micro / 1000000),
    {H, M, Seconds};
to_hms(_) ->
    {0, 0, 0}.

get_tspecs_do(all, Cmd, CCmd) ->
    [
        {cowboy_websocket, websocket_send, [{[{binary, '_'}, '_'], [], [{return_trace}]}]},
        {cowboy_websocket, websocket_send_many, [{['_', '_'], [], [{return_trace}]}]},
        {ege_tcp_handler, send, [{['_', '_'], [], [{return_trace}]}]}
        | ege_msg_ms(Cmd, CCmd)
    ];
get_tspecs_do(only_receive, Cmd, CCmd) ->
    ege_msg_ms(Cmd, CCmd);
get_tspecs_do(only_send, _Cmd, _CCmd) ->
    [
        {cowboy_websocket, websocket_send, [{[{binary, '_'}, '_'], [], [{return_trace}]}]},
        {cowboy_websocket, websocket_send_many, [{['_', '_'], [], [{return_trace}]}]},
        {ege_tcp_handler, send, [{['_', '_'], [], [{return_trace}]}]}
    ].

ege_msg_ms('_', '_') ->
    Ms = dbg:fun2ms(fun([Cmd16, '_', '_', '_']) when Cmd16 =/= 0 -> return_trace() end),
    [{ege_msg, route_lv1, Ms}];
ege_msg_ms(Cmd, '_') ->
    StartCmd16 = ege_msg:encode_cmd(Cmd, 0),
    EndCmd16 = ege_msg:encode_cmd(Cmd + 1, 0) - 1,
    Ms = dbg:fun2ms(fun([Cmd16, '_', '_', '_']) when
        Cmd16 >= StartCmd16 andalso Cmd16 =< EndCmd16 -> return_trace() end),
    [{ege_msg, route_lv1, Ms}];
ege_msg_ms(Cmd, CCmd) ->
    Cmd16 = ege_msg:encode_cmd(Cmd, CCmd),
    Ms = [{[Cmd16, '_', '_', '_'], [], [{return_trace}]}],
    [{ege_msg, route_lv1, Ms}].

decode_send(RespBinaryList) when is_list(RespBinaryList) ->
    case decode_send_list(RespBinaryList, []) of
        [] -> ignore;
        StrList ->
            {"[sendlist] [~ts]", [string:join(StrList, ",")]}
    end;
decode_send(RespBinary) ->
    case do_decode_send(RespBinary) of
        [] -> ignore;
        ignore -> ignore;
        Str ->
            {"[send] ~ts", [Str]}
    end.

do_decode_send({binary, RespBinary}) ->
    do_decode_send(RespBinary);
do_decode_send(<<?CMD_MSG_BASE_HEARTBEAT_RESP:16, _Binary/binary>>) ->
    ignore;
do_decode_send(<<Cmd16:16, _/binary>> = RespBinary) ->
    Output = ?MSG_ROUTER_MODULE:decode_resp_msg(RespBinary),
    io_lib:format("{~p ~ts}", [Cmd16, recon_trace:format_trace_output(Output)]);
do_decode_send(_Other) -> ignore.

decode_send_list([RespBinary | RespBinaryList], List) ->
    case do_decode_send(RespBinary) of
        ignore ->
            decode_send_list(RespBinaryList, List);
        Res ->
            decode_send_list(RespBinaryList, [Res | List])
    end;
decode_send_list([], List) -> List.