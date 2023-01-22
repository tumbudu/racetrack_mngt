%%%-------------------------------------------------------------------
%% @doc racetrack_mngt public API
%% @end
%%%-------------------------------------------------------------------

-module(racetrack_mngt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    racetrack_mngt_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
