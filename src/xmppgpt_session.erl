-module(xmppgpt_session).

-include_lib("exmpp/include/exmpp.hrl").
-include_lib("exmpp/include/exmpp_client.hrl").

-export([start/0, start/5, stop/1]).
-export([init/5]).

start() ->
    start("localhost", 5222, "chatgpt", "localhost", "password").

start(Server, Port, Username, Domain, Password) ->
    spawn(?MODULE, init, [Server, Port, Username, Domain, Password]).

stop(EchoClientPid) ->
    EchoClientPid ! stop.

init(Server, Port, Username, Domain, Password) ->
    %% Start XMPP session: Needed to start service (Like
    %% exmpp_stringprep):
    Session = exmpp_session:start(),
    %% Create XMPP ID (Session Key):
    JID = exmpp_jid:make(Username, Domain, random),
    %% Create a new session with basic (digest) authentication:
    exmpp_session:auth_basic_digest(Session, JID, Password),
    %% Connect in standard TCP:
    {ok, _StreamId} = exmpp_session:connect_TCP(Session, Server, Port),
    session(Session, JID, Password).

%% We are connected. We now log in (and try registering if authentication fails)
session(Session, _MyJID, Password) ->
    %% Login with defined JID / Authentication:
    try exmpp_session:login(Session)
    catch
	throw:{auth_error, 'not-authorized'} ->
	    %% Try creating a new user:
	    io:format("Register~n",[]),
	    %% In a real life client, we should trap error case here
	    %% and print the correct message.
	    exmpp_session:register_account(Session, Password),
	    %% After registration, retry to login:
	    exmpp_session:login(Session)
    end,
    %% We explicitely send presence:
    exmpp_session:send_packet(Session,
			      exmpp_presence:set_status(
				exmpp_presence:available(), "Echo Ready")),
    loop(Session).

%% Process exmpp packet:
loop(Session) ->
    receive
        stop ->
            exmpp_session:stop(Session);
        %% If we receive a message, we reply with the same message
        Record = #received_packet{packet_type=message,
				  raw_packet=Packet,
				  type_attr=Type} when Type =/= "error" ->
            io:format("Received Message stanza:~n~p~n~n", [Record]),
            echo_packet(Session, Packet),
            loop(Session);
	%% If we receive a presence stanza, handle it
	Record when Record#received_packet.packet_type == 'presence' ->
	    io:format("Received Presence stanza:~n~p~n~n", [Record]),
	    handle_presence(Session, Record, Record#received_packet.raw_packet),
	    loop(Session);
        Record ->
            io:format("Received a stanza:~n~p~n~n", [Record]),
            loop(Session)
    end.

%% Send the same packet back for each message received
echo_packet(Session, Packet) ->
    From = exmpp_xml:get_attribute(Packet, <<"from">>, <<"unknown">>),
    To = exmpp_xml:get_attribute(Packet, <<"to">>, <<"unknown">>),
    TmpPacket = exmpp_xml:set_attribute(Packet, <<"from">>, To),
    TmpPacket2 = exmpp_xml:set_attribute(TmpPacket, <<"to">>, From),
    NewPacket = exmpp_xml:remove_attribute(TmpPacket2, <<"id">>),
    exmpp_session:send_packet(Session, NewPacket).

handle_presence(Session, Packet, _Presence) ->
    case exmpp_jid:make(_From = Packet#received_packet.from) of
	JID ->
	    case _Type = Packet#received_packet.type_attr of
		"available" ->
		    %% handle presence availabl
		    ok;
		"unavailable" ->
		    %% handle presence unavailable
		    ok;
		"subscribe" ->
		    presence_subscribed(Session, JID),
		    presence_subscribe(Session, JID);
		"subscribed" ->
		    presence_subscribed(Session, JID),
		    presence_subscribe(Session, JID)
	    end
    end.

presence_subscribed(Session, Recipient) ->
    Presence_Subscribed = exmpp_presence:subscribed(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribed, Recipient),
    exmpp_session:send_packet(Session, Presence).

presence_subscribe(Session, Recipient) ->
    Presence_Subscribe = exmpp_presence:subscribe(),
    Presence = exmpp_stanza:set_recipient(Presence_Subscribe, Recipient),
    exmpp_session:send_packet(Session, Presence).
