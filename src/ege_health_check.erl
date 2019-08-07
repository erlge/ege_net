-module(ege_health_check).

%% API
-export([
    init/2
]).

init(Req0, Opts) ->
    Req = cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, <<"I'm ok!">>, Req0),
    {ok, Req, Opts}.
