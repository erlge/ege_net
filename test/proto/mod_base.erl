-module(mod_base).
-behavior(msg_base).
-include("ege_msg.hrl").
-include("msg_base.hrl").
-include("msg_base_pb.hrl").

%% API
-export([
    heartbeat/2
]).

heartbeat(#heartbeat_req{id = Id}, State) ->
    #{
        msg_list => [#heartbeat_resp{id = Id}],
        state => State#net_handler{last_heartbeat = State#net_handler.last_heartbeat + 1}
    }.