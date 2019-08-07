%%%-------------------------------------------------------------------
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ege_net_ct_hook).

%% API
-export([
    init/2,
    terminate/1
]).

init(Id, _Opts) ->
    {ok, _} = application:ensure_all_started(ege_net),
    {ok, _} = application:ensure_all_started(gun),
    ege_trace_msg:trace_msg(),
    {ok, Id}.

terminate(_) ->
    ege_trace_msg:close_trace(),
    Apps = [ege_net, cowboy, recon, gproc],
    [application:stop(App) || App <- Apps],
    ok.