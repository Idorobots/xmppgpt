-module(xmppgpt_session).
-behaviour(gen_statem).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/5, stop/0]).
-export([init/1, terminate/3, callback_mode/0]).
-export([online/3]).

-define(METHOD, "PLAIN").

start(Server, Port, Username, Domain, Password) ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [Server, Port, Username, Domain, Password], []).

stop() ->
    gen_statem:stop({local, ?MODULE}).

%% Callbacks
callback_mode() ->
    state_functions.

init([Server, Port, Username, Domain, Password]) ->
    Session = exmpp_session:start({1,0}),
    JID = exmpp_jid:make(Username, Domain, random),
    exmpp_session:auth(Session, JID, Password, ?METHOD),
    {ok, _StreamId, _Features} = exmpp_session:connect_SSL(Session, Server, Port),

    try exmpp_session:login(Session, ?METHOD)
    catch
      throw:{auth_error, 'not-authorized'} ->
        io:format("Registering user...~n",[]),
        exmpp_session:register_account(Session, Password),
        exmpp_session:login(Session, ?METHOD)
    end,
    exmpp_session:send_packet(Session, exmpp_presence:set_status(exmpp_presence:available(), "ChatGPT Ready")),
    {ok, online, Session}.

terminate(_Reason, _State, Session) ->
    exmpp_session:stop(Session),
    ok.

online(info, Record = #received_packet{packet_type=message, raw_packet=Packet, type_attr=Type}, Session) when Type =/= "error" ->
    io:format("Received Message stanza:~n~p~n~n", [Record]),
    handle_packet(Session, Packet),
    {keep_state, Session};

online(info, Record, Session) when Record#received_packet.packet_type == 'presence' ->
    io:format("Received Presence stanza:~n~p~n~n", [Record]),
    handle_presence(Session, Record, Record#received_packet.raw_packet),
    {keep_state, Session};

online(info, Record, Session) ->
    io:format("Received a stanza:~n~p~n~n", [Record]),
    {keep_state, Session};

online(EventType, _EventContent, Session) ->
    io:format("Got unknown event type: ~n~p~n~n", [EventType]),
    {keep_state, Session}.

%% Logic
handle_packet(Session, Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, <<"id">>),
    exmpp_session:send_packet(Session, NewPacket).

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
    end.

presence_subscribed(Session, Recipient) ->
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    exmpp_session:send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    exmpp_session:send_packet(Session, Presence).
