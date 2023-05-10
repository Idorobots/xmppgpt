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
init(Args) ->
    io:format("Started with: ~p~n", [Args]),
    ok = hackney_pool:start_pool(?MODULE, [{timeout, ?POOL_TIMEOUT}]),
    {ok, Args}.

terminate(_Reason, _State) ->
    hackney_pool:terminate(?MODULE),
    ok.

handle_call(Request, _From, State) ->
    io:format("Got unknown request: ~n~p~n~n", [Request]),
    {noreply, State}.

handle_cast({prompt, Id, Prompt, From}, State) ->
    io:format("Got a prompt request: ~n~p~n~n", [Prompt]),
    send_request(From, Id, Prompt, State),
    {noreply, State};

handle_cast(Request, State) ->
    io:format("Got unknown request: ~n~p~n~n", [Request]),
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
    Options = [{connect_timeout, ?CONNECT_TIMEOUT}, {recv_timeout, ?REQUEST_TIMEOUT}, {pool, ?MODULE}],
    case hackney:post(Url, Headers, Body, Options) of
        {ok, _StatusCode, _RespHeaders, ClientRef} ->
            {ok, Response} = hackney:body(ClientRef),
            {ok, Parsed, _Rest} = jsone:try_decode(Response),
            handle_response(From, Id, Parsed);

        {error, Reason} ->
            From ! {prompt_response, Id, Reason}
    end.

handle_response(From, Id, #{<<"error">> := #{<<"message">> := Error}}) ->
    From ! {propmt_response, Id, Error};

handle_response(From, Id, #{<<"choices">> := Responses}) ->
    Response = lists:foldl(fun combine_responses/2, <<"">>, Responses),
    From ! {prompt_response, Id, Response};

handle_response(From, Id, Response) ->
    io:format("Got unknown resopnse: ~n~p~n~n", [Response]),
    From ! {prompt_response, Id, "Got an unknown response from ChatGPT."}.

combine_responses(#{<<"message">> := #{<<"content">> := Response}}, Acc) ->
    <<Acc/binary, Response/binary>>;

combine_responses(_Response, Acc) ->
    Acc.
