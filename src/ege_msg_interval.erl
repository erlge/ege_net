%%%-------------------------------------------------------------------
%%% @doc
%%% 用于读取配置生成协议限制模块
%%% @end
%%%-------------------------------------------------------------------
-module(ege_msg_interval).
-include_lib("east_helper/include/ast_helper.hrl").
-include("ege_net.hrl").

%% API
-export([
    init_config/0, set_config/1,
    trans_defines/1, trans_define/1
]).

-type err_lvl() :: atom().
-type cmd() :: atom() | non_neg_integer().
-type ccmd() :: atom() | non_neg_integer().
-type do() :: true | no_limit | pass | handle_pass | err_lvl().
-type interval() :: MilliSecond :: non_neg_integer().
-type define_map() :: #{do := do(), cmd => cmd(), ccmd => ccmd(), cmd16 => ege_msg:cmd16(), interval => interval()}.

%%%===================================================================
%%% API functions
%%%===================================================================

init_config() ->
    case application:get_env(msg_interval) of
        undefined -> skip;
        {ok, disable} ->
            set_config([]);
        {ok, MsgIntervalList} ->
            set_config(MsgIntervalList)
    end.

set_config(Config) when is_list(Config) ->
    persistent_term:put(msg_interval_config, trans_defines(Config));
set_config(Config) ->
    error({error, {msg_interval_not_a_list, Config}}).

-spec trans_defines(Defines :: [define_map(), ...]) -> Config :: list().
trans_defines(Defines) ->
    [trans_define(Define) || Define <- Defines].

%% msg_interval(Cmd, CCmd)
trans_define(#{cmd := CmdName, ccmd := CCmdName} = Define) when is_atom(CmdName) andalso is_atom(CCmdName) ->
    Mod = maps:get(trans_mod, Define, ?MSG_ROUTER_MODULE),
    Cmd16 = Mod:msg_cmd16(CCmdName),
    {Cmd16, trans_do(Define)};
trans_define(#{cmd16 := Cmd16} = Define) when is_integer(Cmd16) ->
    {Cmd16, trans_do(Define)};
%% msg_interval(Cmd, _CCmd)
trans_define(#{cmd := CmdName} = Define) when is_atom(CmdName) ->
    Mod = maps:get(trans_mod, Define, ?MSG_ROUTER_MODULE),
    Cmd = Mod:msg_cmd(CmdName),
    StartCmd16 = ege_msg:encode_cmd(Cmd, 0),
    EndCmd16 = ege_msg:encode_cmd(Cmd + 1, 0) - 1,
    {StartCmd16, EndCmd16, trans_do(Define)};
trans_define(#{cmd := Cmd} = Define) when is_integer(Cmd) ->
    StartCmd16 = ege_msg:encode_cmd(Cmd, 0),
    EndCmd16 = ege_msg:encode_cmd(Cmd + 1, 0) - 1,
    {StartCmd16, EndCmd16, trans_do(Define)};
%% default msg_interval(_Cmd, _CCmd)
trans_define(Define) ->
    {default, trans_do(Define)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

trans_do(#{do := pass, interval := TimeMs}) when is_integer(TimeMs) andalso TimeMs > 0 ->
    {TimeMs, pass};
trans_do(#{do := handle_pass, interval := TimeMs}) when is_integer(TimeMs) andalso TimeMs > 0 ->
    {TimeMs, handle_pass};
trans_do(#{do := true}) -> %% means no_limit
    true;
trans_do(#{do := no_limit}) ->
    true;
trans_do(#{do := ErrLvl, interval := TimeMs} = Define) when is_atom(ErrLvl)
    andalso is_integer(TimeMs) andalso TimeMs > 0 ->
    IsPass = maps:get(is_pass, Define, false),
    {TimeMs, {IsPass, ?EGEN_LEVEL2NUM(ErrLvl)}}.