%% -*- coding: utf-8 -*-
%% Automatically generated, do not edit
%% Generated by gpb_compile version 3.27.5
-module(msg_pb).

-export([encode_msg/1, encode_msg/2]).
-export([decode_msg/2, decode_msg/3]).
-export([merge_msgs/2, merge_msgs/3]).
-export([verify_msg/1, verify_msg/2]).
-export([get_msg_defs/0]).
-export([get_msg_names/0]).
-export([get_group_names/0]).
-export([get_msg_or_group_names/0]).
-export([get_enum_names/0]).
-export([find_msg_def/1, fetch_msg_def/1]).
-export([find_enum_def/1, fetch_enum_def/1]).
-export([enum_symbol_by_value/2, enum_value_by_symbol/2]).
-export([enum_symbol_by_value_cmd/1, enum_value_by_symbol_cmd/1]).
-export([get_service_names/0]).
-export([get_service_def/1]).
-export([get_rpc_names/1]).
-export([find_rpc_def/2, fetch_rpc_def/2]).
-export([get_package_name/0]).
-export([gpb_version_as_string/0, gpb_version_as_list/0]).

-include("msg_pb.hrl").
-include("gpb.hrl").



-spec encode_msg(_) -> no_return().
encode_msg(Msg) -> encode_msg(Msg, []).


-spec encode_msg(_,_) -> no_return().
encode_msg(_Msg, _Opts) ->
    erlang:error({gpb_error, no_messages}).







-spec decode_msg(binary(), atom()) -> no_return().
decode_msg(Bin, _MsgName) when is_binary(Bin) ->
    erlang:error({gpb_error, no_messages}).

-spec decode_msg(binary(), atom(), list()) -> no_return().
decode_msg(Bin, _MsgName, _Opts) when is_binary(Bin) ->
    erlang:error({gpb_error, no_messages}).








-spec merge_msgs(_, _) -> no_return().
merge_msgs(Prev, New) -> merge_msgs(Prev, New, []).

-spec merge_msgs(_, _, _) -> no_return().
merge_msgs(_Prev, _New, _Opts) ->
    erlang:error({gpb_error, no_messages}).



-spec verify_msg(_) -> no_return().
verify_msg(Msg) -> verify_msg(Msg, []).

-spec verify_msg(_,_) -> no_return().
verify_msg(Msg, _Opts) ->
    mk_type_error(not_a_known_message, Msg, []).



-spec mk_type_error(_, _, list()) -> no_return().
mk_type_error(Error, ValueSeen, Path) ->
    Path2 = prettify_path(Path),
    erlang:error({gpb_type_error,
		  {Error, [{value, ValueSeen}, {path, Path2}]}}).


prettify_path([]) -> top_level.






get_msg_defs() -> [{{enum, cmd}, [{msg_base, 0}]}].


get_msg_names() -> [].


get_group_names() -> [].


get_msg_or_group_names() -> [].


get_enum_names() -> [cmd].


-spec fetch_msg_def(_) -> no_return().
fetch_msg_def(MsgName) ->
    erlang:error({no_such_msg, MsgName}).


fetch_enum_def(EnumName) ->
    case find_enum_def(EnumName) of
      Es when is_list(Es) -> Es;
      error -> erlang:error({no_such_enum, EnumName})
    end.


find_msg_def(_) -> error.


find_enum_def(cmd) -> [{msg_base, 0}];
find_enum_def(_) -> error.


enum_symbol_by_value(cmd, Value) ->
    enum_symbol_by_value_cmd(Value).


enum_value_by_symbol(cmd, Sym) ->
    enum_value_by_symbol_cmd(Sym).


enum_symbol_by_value_cmd(0) -> msg_base.


enum_value_by_symbol_cmd(msg_base) -> 0.


get_service_names() -> [].


get_service_def(_) -> error.


get_rpc_names(_) -> error.


find_rpc_def(_, _) -> error.



-spec fetch_rpc_def(_, _) -> no_return().
fetch_rpc_def(ServiceName, RpcName) ->
    erlang:error({no_such_rpc, ServiceName, RpcName}).


get_package_name() -> undefined.



gpb_version_as_string() ->
    "3.27.5".

gpb_version_as_list() ->
    [3,27,5].
