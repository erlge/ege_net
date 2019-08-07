%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ege_net_tests).

-include_lib("eunit/include/eunit.hrl").
-include("ege_net.hrl").

apply_ports_test_() ->
    [
        % name port
        ?_assertEqual([], ege_net:apply_ports([{a, 1}, {b, 2}], [])),
        ?_assertEqual([
            {a, #{trans_opts => [{socket_opts, [{port, 0}]}]}}
        ], ege_net:apply_ports([], [
            {a, #{trans_opts => [{socket_opts, [{port, 0}]}]}}
        ])),
        ?_assertEqual([
            {a, #{trans_opts => [{socket_opts, [{port, 1}]}]}}
        ], ege_net:apply_ports([{a, 1}, {b, 2}], [
            {a, #{trans_opts => []}}
        ])),
        ?_assertEqual([
            {a, #{trans_opts => [{socket_opts, [{port, 1}]}]}}
        ], ege_net:apply_ports([{a, 1}, {b, 2}], [
            {a, #{trans_opts => [{socket_opts, [{port, 0}]}]}}
        ])),
        % port
        ?_assertEqual([
            {a, #{trans_opts => [{socket_opts, [{port, 1}]}]}},
            {b, #{trans_opts => [{socket_opts, [{port, 2}]}]}},
            {c, #{trans_opts => [{socket_opts, [{port, 3}]}]}},
            {d, #{trans_opts => []}}
        ], ege_net:apply_ports([1, 2, 3], [
            {a, #{trans_opts => []}},
            {b, #{trans_opts => []}},
            {c, #{trans_opts => []}},
            {d, #{trans_opts => []}}
        ])),
        ?_assertEqual([
            {a, #{trans_opts => [{socket_opts, [{port, 1}]}]}},
            {b, #{trans_opts => [{socket_opts, [{port, 2}]}]}}
        ], ege_net:apply_ports([1, 2, 3], [
            {a, #{trans_opts => []}},
            {b, #{trans_opts => []}}
        ])),
        ?_assertEqual([
            {a, #{trans_opts => [{socket_opts, [{port, 10}]}]}},
            {b, #{trans_opts => [{socket_opts, [{port, 1}]}]}},
            {c, #{trans_opts => [{socket_opts, [{port, 2}]}]}},
            {d, #{trans_opts => [{socket_opts, [{port, 3}]}]}}
        ], ege_net:apply_ports([1, 2, 3], [
            {a, #{trans_opts => [{socket_opts, [{port, 10}]}]}},
            {b, #{trans_opts => []}},
            {c, #{trans_opts => []}},
            {d, #{trans_opts => []}}
        ])),
        % name port & port
        ?_assertEqual([
            {a, #{trans_opts => [{socket_opts, [{port, 10}]}]}},
            {b, #{trans_opts => [{socket_opts, [{port, 1}]}]}},
            {c, #{trans_opts => [{socket_opts, [{port, 3}]}]}},
            {d, #{trans_opts => [{socket_opts, [{port, 2}]}]}}
        ], ege_net:apply_ports([1, {d, 2}, 3], [
            {a, #{trans_opts => [{socket_opts, [{port, 10}]}]}},
            {b, #{trans_opts => []}},
            {c, #{trans_opts => []}},
            {d, #{trans_opts => []}}
        ]))
    ].

handle_trans_opts_test() ->
    %% port
    ?assertEqual({error, not_define_port}, ege_net:handle_trans_opts([], #{})),
    ?assertEqual({error, {error_port, ""}}, ege_net:handle_trans_opts([{socket_opts, [{port, ""}]}], #{})),
    ?assertEqual({error, {error_port, -1}}, ege_net:handle_trans_opts([{socket_opts, [{port, -1}]}], #{})),
    ?assertEqual({ok, #{socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{socket_opts, [{port, 0}]}], #{})),
    ?assertEqual({ok, #{socket_opts => [{port, 1111}]}},
        ege_net:handle_trans_opts([{socket_opts, [{port, 1111}]}], #{})),
    ?assertEqual({ok, #{socket_opts => [{port, 1111}]}},
        ege_net:handle_trans_opts([{socket_opts, [{port, "1111"}]}], #{})),
    ?assertEqual({ok, #{socket_opts => [{port, 1111}]}},
        ege_net:handle_trans_opts([{socket_opts, [{port, <<"1111">>}]}], #{})),

    %% num_acceptors
    ?assertEqual({error, {error_num_acceptors, ""}}, ege_net:handle_trans_opts([{num_acceptors, ""}], #{})),
    ?assertEqual({error, {error_num_acceptors, -1}}, ege_net:handle_trans_opts([{num_acceptors, -1}], #{})),

    ?assertEqual({ok, #{num_acceptors => 10, socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{num_acceptors, 10}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{num_acceptors => 10, socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{num_acceptors, "10"}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{num_acceptors => 10, socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{num_acceptors, <<"10">>}], #{socket_opts => [{port, 0}]})),

    %% max_connections
    ?assertEqual({error, {error_max_connections, -1}}, ege_net:handle_trans_opts([{max_connections, -1}], #{})),
    ?assertEqual({ok, #{max_connections => infinity, socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{max_connections, infinity}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{max_connections => infinity, socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{max_connections, "infinity"}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{max_connections => 10000, socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{max_connections, 10000}], #{socket_opts => [{port, 0}]})),

    %% certfile & keyfile
    PrivDir = code:priv_dir(?APP_NAME),
    meck:new(filelib, [unstick]),
    meck:expect(filelib, is_file, 1, true),
    ?assertEqual({ok, #{certfile => "test", socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{certfile, "test"}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{certfile => PrivDir ++ "/ssl/server.crt", socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{certfile, {?APP_NAME, priv}}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{certfile => PrivDir ++ "/ssl/server.crt", socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{certfile, {?APP_NAME, "priv"}}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{certfile => PrivDir ++ "/test", socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{certfile, {?APP_NAME, "test"}}], #{socket_opts => [{port, 0}]})),

    ?assertEqual({ok, #{keyfile => "test", socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{keyfile, "test"}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{keyfile => PrivDir ++ "/ssl/server.key", socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{keyfile, {?APP_NAME, priv}}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{keyfile => PrivDir ++ "/ssl/server.key", socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{keyfile, {?APP_NAME, "priv"}}], #{socket_opts => [{port, 0}]})),
    ?assertEqual({ok, #{keyfile => PrivDir ++ "/test", socket_opts => [{port, 0}]}},
        ege_net:handle_trans_opts([{keyfile, {?APP_NAME, "test"}}], #{socket_opts => [{port, 0}]})),

    meck:expect(filelib, is_file, 1, false),
    ?assertEqual({error, {certfile, {error_file, {?APP_NAME, "test"}}}},
        ege_net:handle_trans_opts([{certfile, {?APP_NAME, "test"}}], #{})),
    ?assertEqual({error, {keyfile, {error_file, {?APP_NAME, "test"}}}},
        ege_net:handle_trans_opts([{keyfile, {?APP_NAME, "test"}}], #{})),
    ?assertEqual({error, {keyfile, {error_file, "test"}}},
        ege_net:handle_trans_opts([{keyfile, "test"}], #{})),
    meck:unload(filelib),

    %% other
    ?assertEqual({ok, #{socket_opts => [{port, 0}], test => 10}},
        ege_net:handle_trans_opts([{socket_opts, [{port, 0}]}, {test, 10}], #{})),
    ok.
