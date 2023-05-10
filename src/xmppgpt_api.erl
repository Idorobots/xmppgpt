-module(xmppgpt_api).
-behaviour(gen_server).

-export([start/5, stop/0]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-export([process_prompt/2]).

-define(POOL_TIMEOUT, 120000).
-define(CONNECT_TIMEOUT, 10000).
-define(REQUEST_TIMEOUT, 30000).

start(ApiUrl, ApiKey, OrgId, Model, Temp) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [ApiUrl, ApiKey, OrgId, Model, Temp], []).

stop() ->
    gen_server:stop(?MODULE).

process_prompt(Id, Prompt) ->
    gen_server:cast(?MODULE, {prompt, Id, Prompt, self()}).

%% Callbacks
init([_Url, _ApiKey, OrgId | _ ] = Args) ->
    ok = lager:info("Started with organization: ~p~n", [OrgId]),
    ok = hackney_pool:start_pool(?MODULE, [{timeout, ?POOL_TIMEOUT}]),
    {ok, Args}.

terminate(_Reason, _State) ->
    hackney_pool:stop_pool(?MODULE).

handle_call(Request, _From, State) ->
    ok = lager:warning("Got unknown request: ~n~p~n~n", [Request]),
    {noreply, State}.

handle_cast({prompt, Id, Prompt, From}, State) ->
    ok = lager:debug("Got a prompt request: ~n~p~n~n", [Prompt]),
    _ = send_request(From, Id, Prompt, State),
    {noreply, State};

handle_cast(Request, State) ->
    ok = lager:warning("Got unknown request: ~n~p~n~n", [Request]),
    {noreply, State}.

send_request(From, Id, Prompt, [Url, ApiKey, _OrgId, Model, Temp]) ->
    Json = #{
      <<"model">> => list_to_binary(Model),
      <<"temperature">> => Temp,
      <<"messages">> => [#{
        <<"role">> => <<"user">>,
        <<"content">> => Prompt
      }]
    },
    Body = jsone:encode(Json),
    ApiKeyBin = list_to_binary(ApiKey),
    Headers = [
        {<<"Authorization">>, <<"Bearer ", ApiKeyBin/binary>>},
        {<<"Content-Type">>, <<"application/json">>}
    ],
    Options = [
        {connect_timeout, ?CONNECT_TIMEOUT},
        {recv_timeout, ?REQUEST_TIMEOUT},
        {pool, ?MODULE}
    ],
    _ = case hackney:request(post, Url, Headers, Body, Options) of
        {ok, _StatusCode, _RespHeaders, ClientRef} ->
            {ok, Response} = hackney:body(ClientRef),
            {ok, Parsed, _Rest} = jsone:try_decode(Response),
            handle_response(From, Id, Parsed);

        {error, Reason} ->
            From ! {prompt_response, Id, Reason}
    end.

handle_response(From, Id, #{<<"error">> := #{<<"message">> := Error}}) ->
    From ! {prompt_response, Id, Error};

handle_response(From, Id, #{<<"choices">> := Responses}) ->
    Response = lists:foldl(fun combine_responses/2, <<"">>, Responses),
    From ! {prompt_response, Id, Response};

handle_response(From, Id, Response) ->
    ok = lager:warning("Got unknown resopnse: ~n~p~n~n", [Response]),
    From ! {prompt_response, Id, "Got an unknown response from ChatGPT."}.

combine_responses(#{<<"message">> := #{<<"content">> := Response}}, Acc) ->
    <<Acc/binary, Response/binary>>;

combine_responses(_Response, Acc) ->
    Acc.
