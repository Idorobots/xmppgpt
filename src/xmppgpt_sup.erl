-module(xmppgpt_sup).

-behaviour(supervisor).

-export([start_link/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link(SessionArgs, ChatArgs) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, [SessionArgs, ChatArgs]).

init([SessionArgs, ChatArgs]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
      #{
        id => xmppgpt_session,
        start => {xmppgpt_session, start, SessionArgs},
        shutdown => brutal_kill,
        type => worker,
        module => [xmppgpt_session]
      },
      #{
        id => xmppgpt_api,
        start => {xmppgpt_api, start, ChatArgs},
        shutdown => brutal_kill,
        type => worker,
        module => [xmppgpt_api]
      }],
    {ok, {SupFlags, ChildSpecs}}.
