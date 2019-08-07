%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ege_net_limit_tests).
-include_lib("eunit/include/eunit.hrl").
-include("ege_msg.hrl").

generate_test() ->
    meck:new(msg_router, [non_strict]),

    %% 验证传入值
    ?assertError({error, {msg_interval_not_a_list, aa}}, ege_msg_interval:set_config(aa)),
    %% 验证是否生成成功
    Config1 = ege_msg_interval:trans_defines([
        #{do => info, interval => 100}
    ]),

    ?assertEqual({100, {false, ?EGEN_LEVEL2NUM(info)}}, ege_msg:interval_config(ege_msg:encode_cmd(1, 1), Config1)),
    %% 验证is_pass=true
    Config2 = ege_msg_interval:trans_defines([
        #{do => info, interval => 100, is_pass => true}
    ]),
    ?assertEqual({100, {true, ?EGEN_LEVEL2NUM(info)}}, ege_msg:interval_config(ege_msg:encode_cmd(1, 1), Config2)),

    meck:expect(msg_router, msg_cmd, fun(msg_test) -> 1 end),
    %% 验证cmd
    Config3 = ege_msg_interval:trans_defines([
        #{cmd => msg_test, do => info, interval => 1000},
        #{do => error, interval => 100, is_pass => true}
    ]),
    ?assertEqual({1000, {false, ?EGEN_LEVEL2NUM(info)}}, ege_msg:interval_config(ege_msg:encode_cmd(1, 1), Config3)),
    ?assertEqual({100, {true, ?EGEN_LEVEL2NUM(error)}}, ege_msg:interval_config(ege_msg:encode_cmd(2, 2), Config3)),

    meck:expect(msg_router, msg_cmd16, fun(msg_test_req) -> 513 end),
    %% 验证cmd & ccmd
    Config4 = ege_msg_interval:trans_defines([
        #{cmd => msg_test, ccmd => msg_test_req, do => debug, interval => 2000},
        #{cmd => msg_test, do => info, interval => 1000},
        #{do => error, interval => 100, is_pass => true}
    ]),
    ?assertEqual({2000, {false, ?EGEN_LEVEL2NUM(debug)}}, ege_msg:interval_config(ege_msg:encode_cmd(1, 1), Config4)),
    ?assertEqual({1000, {false, ?EGEN_LEVEL2NUM(info)}}, ege_msg:interval_config(ege_msg:encode_cmd(1, 2), Config4)),
    ?assertEqual({100, {true, ?EGEN_LEVEL2NUM(error)}}, ege_msg:interval_config(ege_msg:encode_cmd(2, 2), Config4)),
    %% 验证pass
    Config5 = ege_msg_interval:trans_defines([
        #{do => pass, interval => 100}
    ]),
    ?assertEqual({100, pass}, ege_msg:interval_config(ege_msg:encode_cmd(1, 1), Config5)),
    %% 验证handle_pass
    Config6 = ege_msg_interval:trans_defines([
        #{do => handle_pass, interval => 100}
    ]),
    ?assertEqual({100, handle_pass}, ege_msg:interval_config(ege_msg:encode_cmd(1, 1), Config6)),
    %% 验证no_limit
    Config7 = ege_msg_interval:trans_defines([#{do => no_limit}]),
    ?assertEqual(true, ege_msg:interval_config(ege_msg:encode_cmd(1, 1), Config7)),
    %% 验证true
    Config8 = ege_msg_interval:trans_defines([#{do => true}]),
    ?assertEqual(true, ege_msg:interval_config(ege_msg:encode_cmd(1, 1), Config8)),

    meck:unload(msg_router),
    ok.
