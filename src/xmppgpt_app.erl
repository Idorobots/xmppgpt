-module(xmppgpt_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, StartArgs) ->
    lager:info("Started with: ~p~n", [StartArgs]),
    {ok, Server} = application:get_env(server),
    {ok, Port} = application:get_env(port),
    {ok, Username} = application:get_env(username),
    {ok, Domain} = application:get_env(domain),
    {ok, Password} = application:get_env(password),
    {ok, ChatApiUrl} = application:get_env(chat_api_url),
    {ok, ChatApiModel} = application:get_env(chat_api_model),
    {ok, ChatApiTemp} = application:get_env(chat_api_temperature),
    ChatApiKey = os:getenv("OPENAI_KEY", "scratch-me-because-i-didnt-set-OPENAI_KEY-env-var"),
    ChatApiOrg = os:getenv("OPENAI_ORG", "scratch-me-because-i-didnt-set-OPENAI_ORG-env-var"),
    xmppgpt_sup:start_link(
        [Server, Port, Username, Domain, Password],
        [ChatApiUrl, ChatApiKey, ChatApiOrg, ChatApiModel, ChatApiTemp]
    ).

stop(_State) ->
    ok.
