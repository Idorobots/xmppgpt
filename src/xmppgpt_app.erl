%%%-------------------------------------------------------------------
%% @doc xmppgpt public API
%% @end
%%%-------------------------------------------------------------------

-module(xmppgpt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) ->
    io:format("Started with: ~p~n", [StartArgs]),
    {ok, Server} = application:get_env(server),
    {ok, Port} = application:get_env(port),
    {ok, Username} = application:get_env(username),
    {ok, Domain} = application:get_env(domain),
    {ok, Password} = application:get_env(password),
    xmppgpt_sup:start_link([Server, Port, Username, Domain, Password]).

stop(_State) ->
    ok.

%% internal functions
