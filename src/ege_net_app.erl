%%%-------------------------------------------------------------------
%%% @doc
%%% application
%%% @end
%%%-------------------------------------------------------------------
-module(ege_net_app).
-behaviour(application).

%% Application callbacks
-export([
    start/2,
    stop/1
]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

start(_StartType, _StartArgs) ->
    case ege_net_sup:start_link() of
        {ok, Sup} ->
            ege_msg_interval:init_config(),
            {ok, Sup};
        Err -> Err
    end.

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
