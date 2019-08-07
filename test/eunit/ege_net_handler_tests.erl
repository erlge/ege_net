%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ege_net_handler_tests).
-include("ege_net.hrl").
-include_lib("eunit/include/eunit.hrl").

send_test_() ->
    Return1 = ege_net_handler:send(undefined, <<"test">>),
    Return2 = ege_net_handler:send(self(), <<"test">>),
    receive
        Send2 -> Send2
    end,
    Return3 = ege_net_handler:send(self(), []),
    ege_net_handler:send(self(), [<<"test1">>, <<"test2">>]),
    receive
        Send4 -> Send4
    end,
    [
        ?_assertEqual(undefined, Return1),
        ?_assertEqual(ok, Return2),
        ?_assertEqual({send, <<"test">>}, Send2),
        ?_assertEqual(ok, Return3),
        ?_assertEqual({send, [<<"test1">>, <<"test2">>]}, Send4)
    ].

send_msg_test_() ->
    Return1 = ege_net_handler:send_msg(undefined, {}),
    meck:new(msg_router, [non_strict]),
    meck:expect(msg_router, encode_resp_msg, 1, <<"test">>),
    Return2 = ege_net_handler:send_msg(self(), {test, a, b, c}),
    receive
        Send -> Send
    end,
    meck:unload(msg_router),
    [
        ?_assertEqual(undefined, Return1),
        ?_assertEqual(ok, Return2),
        ?_assertEqual({send, <<"test">>}, Send)
    ].

stop_test_() ->
    Return1 = ege_net_handler:stop(undefined),
    Return2 = ege_net_handler:stop(self()),
    receive
        Stop1 -> Stop1
    end,
    Return3 = ege_net_handler:stop(undefined, <<>>),
    Return4 = ege_net_handler:stop(self(), <<"test">>),
    receive
        Stop2 -> Stop2
    end,
    [
        ?_assertEqual(undefined, Return1),
        ?_assertEqual(ok, Return2),
        ?_assertEqual(stop, Stop1),
        ?_assertEqual(undefined, Return3),
        ?_assertEqual(ok, Return4),
        ?_assertEqual({stop, <<"test">>}, Stop2)
    ].