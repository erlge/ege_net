%%%-------------------------------------------------------------------
%%% @doc
%%% net最上级监督进程
%%% @end
%%%-------------------------------------------------------------------
-module(ege_net_sup).
-behaviour(supervisor).

-export([
    start_link/0,
    init/1
]).

%%%===================================================================
%%% API functions
%%%===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([]) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    Children = [
        %% 维护所有的handler
        #{id => ege_net_handler, start => {ege_net_handler, start_link, []}, modules => [ege_net_handler],
            restart => permanent, shutdown => 5000, type => worker}
    ],
    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
