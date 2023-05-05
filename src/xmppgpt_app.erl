%%%-------------------------------------------------------------------
%% @doc xmppgpt public API
%% @end
%%%-------------------------------------------------------------------

-module(xmppgpt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    xmppgpt_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
