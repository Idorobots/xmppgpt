-module(xmppgpt_api).
-behaviour(gen_server).

-export([start/2, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([process_prompt/2]).

start(ApiUrl, ApiKey) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ApiUrl, ApiKey], []).

stop() ->
    gen_server:stop(?MODULE).

process_prompt(Id, Prompt) ->
    gen_server:cast(?MODULE, {prompt, Id, Prompt, self()}).

%% Callbacks
init([ApiUrl, ApiKey]) ->
    {ok, []}.

terminate(_Reason, _State) ->
    ok.

handle_call(Request, _From, State) ->
    io:format("Got unknown request: ~n~p~n~n", [Request]),
    {noreply, State}.

handle_cast({prompt, Id, Prompt, From}, State) ->
    Response = Prompt, %% TODO Actually implement this.
    timer:send_after(5000, From, {prompt_response, Id, Response}),
    {noreply, State};

handle_cast(Request, State) ->
    io:format("Got unknown request: ~n~p~n~n", [Request]),
    {noreply, State}.
