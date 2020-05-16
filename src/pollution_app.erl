%%%-------------------------------------------------------------------
%% @doc pollution public API
%% @end
%%%-------------------------------------------------------------------

-module(pollution_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    pollution_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
