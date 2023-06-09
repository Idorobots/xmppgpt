-module(xmppgpt_session).
-behaviour(gen_statem).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/5, stop/0]).
-export([init/1, terminate/3, callback_mode/0]).
-export([listening/3, awaiting_response/3]).

-define(METHOD, "PLAIN").

start(Server, Port, Username, Domain, Password) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE,
                          [Server, Port, Username, Domain, Password], []).

stop() ->
    gen_statem:stop({local, ?MODULE}).

%% Callbacks
callback_mode() ->
    state_functions.

init([Server, Port, Username, Domain, Password] = Args) ->
    ok = lager:info("Started with: ~p~n", [Args]),
    Session = exmpp_session:start_link({1, 0}),
    JID = exmpp_jid:make(Username, Domain, random),
    exmpp_session:auth(Session, JID, Password, ?METHOD),
    {ok, _StreamId, _Features} = exmpp_session:connect_SSL(Session, Server, Port),

    _ = try exmpp_session:login(Session, ?METHOD)
    catch
      throw:{auth_error, 'not-authorized'} ->
        ok = lager:info("Registering user...~n", []),
        exmpp_session:register_account(Session, Password),
        exmpp_session:login(Session, ?METHOD)
    end,
    Packet = exmpp_presence:set_status(exmpp_presence:available(), "ChatGPT Ready"),
    exmpp_session:send_packet(Session, Packet),
    {ok, listening, Session}.

terminate(_Reason, _State, Session) ->
    exmpp_session:stop(Session),
    ok.

listening(info, #received_packet{
    packet_type=message,
    raw_packet=Packet,
    type_attr=Type
} = Record, Session) when Type =/= "error" ->
    ok = lager:debug("Received Message stanza:~n~p~n~n", [Record]),
    handle_packet(Session, Packet);

listening(info, Record, Session) when Record#received_packet.packet_type == 'presence' ->
    ok = lager:debug("Received Presence stanza:~n~p~n~n", [Record]),
    handle_presence(Session, Record, Record#received_packet.raw_packet);

listening(info, Record, Session) ->
    ok = lager:debug("Received a stanza:~n~p~n~n", [Record]),
    {keep_state, Session};

listening(EventType, _EventContent, Session) ->
    ok = lager:warning("Got unknown event type: ~n~p~n~n", [EventType]),
    {keep_state, Session}.

awaiting_response(info, {prompt_response, {Id, To, From}, Response}, Session) ->
    respond(Session, Id, To, From, Response),
    {next_state, listening, Session};

awaiting_response(info, #received_packet{
    packet_type=message,
    raw_packet=Packet,
    type_attr=Type
} = Record, Session) when Type =/= "error" ->
    ok = lager:debug("Received Message stanza:~n~p~n~n", [Record]),
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    Id = exmpp_xml:get_attribute(Packet, <<"id">>, <<"unknown">>),
    respond(Session, Id, To, From, "Busy..."),
    {keep_state, Session};

awaiting_response(info, Record, Session) when Record#received_packet.packet_type == 'presence' ->
    ok = lager:debug("Received Presence stanza:~n~p~n~n", [Record]),
    handle_presence(Session, Record, Record#received_packet.raw_packet);

awaiting_response(EventType, _EventContent, Session) ->
    ok = lager:warning("Got unknown event type: ~n~p~n~n", [EventType]),
    {keep_state, Session}.

%% Logic
handle_packet(Session, Packet) ->
    Id = exmpp_xml:get_attribute(Packet, <<"id">>, <<"unknown">>),
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    case exmpp_message:get_body(Packet) of
      undefined ->
          %% Typing indication, etc.
          {keep_state, Session};
      Body ->
          ok = lager:debug("Processing ChatGPT prompt: ~n~p~n~n", [Body]),
          xmppgpt_api:process_prompt({Id, To, From}, Body),
          {next_state, awaiting_response, Session}
    end.

respond(Session, Id, From, To, Response) ->
    Message = exmpp_message:chat(Response),
    WithSender = exmpp_stanza:set_sender(Message, From),
    Packet = exmpp_stanza:set_recipient(WithSender, To),
    send_packet(Session, exmpp_stanza:set_id(Packet, Id)).

handle_presence(Session, Packet, _Presence) ->
    JID = exmpp_jid:make(Packet#received_packet.from),
    case Packet#received_packet.type_attr of
        "available" ->
            ok;
        "unavailable" ->
            ok;
        "subscribe" ->
            presence_subscribed(Session, JID),
            presence_subscribe(Session, JID);
        "subscribed" ->
            presence_subscribed(Session, JID),
            presence_subscribe(Session, JID)
    end,
    {keep_state, Session}.

presence_subscribed(Session, Recipient) ->
    PresenceSubscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(PresenceSubscribed, Recipient),
    send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
    PresenceSubscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(PresenceSubscribe, Recipient),
    send_packet(Session, Presence).

send_packet(Session, Packet) ->
    ok = lager:debug("Sending stanza: ~n~p~n~n", [Packet]),
    exmpp_session:send_packet(Session, Packet).
