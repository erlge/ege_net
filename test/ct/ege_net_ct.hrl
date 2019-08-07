
-define(WS_LISTENER_OPTS(Port), #{type => ws, trans_opts => [
    {socket_opts, [{port, Port}]}
]}).
-define(TCP_LISTENER_OPTS(Port), #{type => tcp, trans_opts => [
    {socket_opts, [{port, Port}]}
]}).