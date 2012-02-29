%%%-------------------------------------------------------------------
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2011, Gert Meulyzer
%%% @doc
%%% attempt at IRC client in a gen_server
%%% @end
%%% Created : 28 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(girc).

-behaviour(gen_server).
-include("../include/irc.hrl").

-define(COLON, 58).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).

-export([send_msg/2, send_raw/2, handle/2, handle_numeric_reply/2]).

-define(SERVER, ?MODULE).

-record(state, {socket, 
                host, 
                port=6667, 
                username, 
                callbackmodule, 
                passthrough=false,
                connectionhelper=undefined
               }).

%%%===================================================================
%%% Behaviour definition
%%%===================================================================
-callback handle_msg(Message :: #ircmsg{}) -> Reply :: #ircmsg{} | ok.
-callback handle_join(Channel :: binary(), Nick :: binary(), Msg :: #ircmsg{}) -> Reply :: #ircmsg{} | ok.
-callback handle_quit(Nick :: binary(), Msg :: #ircmsg{}) -> Reply :: #ircmsg{} | ok.
-callback handle_privmsg(From :: binary(), To :: binary(), Msg :: #ircmsg{}) -> Reply :: #ircmsg{} | ok.
%% -callback handle_numeric_reply(Int :: integer(), Msg :: #ircmsg{}) -> Reply :: #ircmsg{} | ok.

%%%===================================================================
%%% API
%%%===================================================================

%% -spec send_msg(Pid :: pid()) -> Msg :: ircmsg:#ircmsg{}.
send_msg(Pid, Msg) ->
    gen_server:cast(Pid, {send_msg, Msg}).
send_raw(Pid, Line) ->
    gen_server:cast(Pid, {send_raw, Line}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Module, Host, Port, UserName) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Module, Host, Port, UserName], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Module, Host, Port, UserName]) ->
    {ok, #state{host=Host, port=Port, username=UserName, callbackmodule=Module}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok, 
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({send_raw, Line}, #state{socket=Sock}=State) ->
    send_rawmsg(Sock, Line), 
    {noreply, State};
handle_cast({send_msg, Msg}, #state{socket=Sock}=State) ->
    send_ircmsg(Sock, Msg), 
    {noreply, State};
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(ping_server, State) ->
    io:format("Pinging...~n"),
    gen_server:cast(self(), {send_raw, <<"PING :DingBotConnCheck">>}),
    {noreply, State};
handle_cast(got_pong, #state{connectionhelper=C}=State) ->
    io:format("Got pong.~n"),
    C ! pong,
    {noreply, State};
handle_cast(no_pong, State) ->
    io:format("Disconnected apparently..."),
    {stop, disconnected, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{host=Host, port=Port, username=UserName, callbackmodule=Module}) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, true}]), 
    %% Do the IRC login
    gen_tcp:send(Sock, "NICK "++UserName), 
    gen_tcp:send(Sock, "\r\n"), 
    gen_tcp:send(Sock, "USER "++UserName++" "++UserName++" "++UserName++" "++UserName), 
    gen_tcp:send(Sock, "\r\n"), 
    %% spawn the helper process to keep pinging the server.
    %% needs to be its own module probably.
    ConHelpPid = spawn_link(connectionhelper, start, [Sock, self()]),
    {noreply, #state{socket=Sock, host=Host, port=Port, username=UserName, callbackmodule=Module, connectionhelper=ConHelpPid}};
handle_info({tcp, _S, Data}, #state{socket=Sock, callbackmodule=Mod, passthrough=PT}=State) ->
    Msg = ircmsg:parse_line(Data), 
    case PT of
        true -> 
            Response = Mod:handle_msg(Msg), 
            case Response of
                none -> ok;
                _ -> send_ircmsg(Sock, Response)
            end;
        false ->
            Response = ?MODULE:handle(Mod, Msg), 
            case Response of
                ok -> ok;
                _ -> send_ircmsg(Sock, Response)
            end
    end, 
    {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
    io:format("DISCONNECTED!!!!"), 
    {stop, disconnected, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    io:format("TCP ERROR! ~p~n", [Reason]), 
    {stop, error, State};
handle_info(Info, State) ->
    io:format("UNKNOWN: ~p~n", [Info]), 
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

send_ircmsg(_Sock, ok) ->
    ok;
send_ircmsg(Sock, Msg) ->
    gen_tcp:send(Sock, iolist_to_binary([ircmsg:to_line(Msg), <<"\r\n">>])).

send_rawmsg(Sock, Line) ->
    gen_tcp:send(Sock, [Line, "\r\n"]).


%%%===================================================================
%%% Main dispatcher
%%%===================================================================

-spec handle(atom(), #ircmsg{}) -> ok.
handle(_Mod, #ircmsg{command= <<"PING">>, tail=T}=_Msg) ->
    ircmsg:create(<<>>, <<"PONG">>, [], T);
handle(_Mod, #ircmsg{prefix=_P, command= <<"PONG">>, arguments=_A, tail=_T}=_Msg) ->
    io:format("Got a pong back, casting to self()~n"),
    gen_server:cast(self(), got_pong);
handle(Mod, #ircmsg{prefix=P, command= <<"JOIN">>, arguments=A, tail=_T}=Msg) ->
    Mod:handle_join(hd(A), ircmsg:nick_from_prefix(P), Msg);
handle(Mod, #ircmsg{prefix=P, command= <<"QUIT">>, arguments=_A, tail=_T}=Msg) ->
    Mod:handle_quit(ircmsg:nick_from_prefix(P), Msg);
handle(Mod, #ircmsg{prefix=P, command= <<"PRIVMSG">>, arguments=A, tail=_T}=Msg) ->
    Mod:handle_privmsg(ircmsg:nick_from_prefix(P), hd(A), Msg);
handle(_Mod, #ircmsg{prefix=_P, command=_C, arguments=_A, tail=_T}=Msg) ->
    case ircmsg:is_numeric(Msg) of
        {true, Nr} -> ?MODULE:handle_numeric_reply(Nr, Msg);
        {false, _} -> io:format("~p~n",[Msg]),
                      ok
    end.
    
%%%===================================================================
%%% Numeric handlers. See: http://www.irchelp.org/irchelp/rfc/rfc2812.txt
%%%===================================================================
%%% Numerics in the range from 001 to 099 are used for client-server
%%% connections only and should never travel between servers.  Replies
%%% generated in the response to commands are found in the range from 200
%%% to 399.
%%%===================================================================
-spec handle_numeric_reply(Nr :: integer(), Msg :: #ircmsg{}) -> #ircmsg{} | ok.

%%% The server sends Replies 001 to 004 to a user upon
%%% successful registration.
%% RPL_WELCOME
handle_numeric_reply(001, _Msg) -> 
    ok;
%% RPL_YOURHOST
handle_numeric_reply(002, _Msg) ->
    ok;
%% RPL_CREATED
handle_numeric_reply(003, _Msg) ->
    ok;
%% RPL_MYINFO
handle_numeric_reply(004, _Msg) ->
    ok;

%% Sent by the server to a user to suggest an alternative
%% server.  This is often used when the connection is
%% refused because the server is already full.

%% RPL_BOUNCE
handle_numeric_reply(005, _Msg) ->
    ok;

%% - Reply format used by USERHOST to list replies to
%%   the query list.  The reply string is composed as
%%   follows:
 
%%   reply = nickname [ "*" ] "=" ( "+" / "-" ) hostname

%%   The '*' indicates whether the client has registered
%%   as an Operator.  The '-' or '+' characters represent
%%   whether the client has set an AWAY message or not
%%   respectively.

%% RPL_USERHOST
handle_numeric_reply(302, _Msg) ->
    ok;

%% - Reply format used by ISON to list replies to the
%%   query list.

%% RPL_ISON
handle_numeric_reply(303, _Msg) ->
    ok;

%% - These replies are used with the AWAY command (if
%% allowed).  RPL_AWAY is sent to any client sending a
%% PRIVMSG to a client which is away.  RPL_AWAY is only
%% sent by the server to which the client is connected.
%% Replies RPL_UNAWAY and RPL_NOWAWAY are sent when the
%% client removes and sets an AWAY message.

%% RPL_AWAY
handle_numeric_reply(301, _Msg) ->
    ok;
%% RPL_UNAWAY
handle_numeric_reply(305, _Msg) ->
    ok;
%% RPL_NOWAWAY
handle_numeric_reply(306, _Msg) ->
    ok;

%% - Replies 311 - 313, 317 - 319 are all replies
%% generated in response to a WHOIS message.  Given that
%% there are enough parameters present, the answering
%% server MUST either formulate a reply out of the above
%% numerics (if the query nick is found) or return an
%% error reply.  The '*' in RPL_WHOISUSER is there as
%% the literal character and not as a wild card.  For
%% each reply set, only RPL_WHOISCHANNELS may appear
%% more than once (for long lists of channel names).
%% The '@' and '+' characters next to the channel name
%% indicate whether a client is a channel operator or
%% has been granted permission to speak on a moderated
%% channel.  The RPL_ENDOFWHOIS reply is used to mark
%% the end of processing a WHOIS message.

%% RPL_WHOISUSER
handle_numeric_reply(311, _Msg) ->
    ok;
%% RPL_WHOISSERVER
handle_numeric_reply(312, _Msg) ->
    ok;
%% RPL_WHOISOPERATOR
handle_numeric_reply(313, _Msg) ->
    ok;
%% RPL_WHOISIDLE
handle_numeric_reply(317, _Msg) ->
    ok;
%% RPL_ENDOFWHOIS
handle_numeric_reply(318, _Msg) ->
    ok;
%% RPL_WHOISCHANNELS
handle_numeric_reply(319, _Msg) ->
    ok;

%% - When replying to a WHOWAS message, a server MUST use
%% the replies RPL_WHOWASUSER, RPL_WHOISSERVER or
%% ERR_WASNOSUCHNICK for each nickname in the presented
%% list.  At the end of all reply batches, there MUST
%% be RPL_ENDOFWHOWAS (even if there was only one reply
%% and it was an error).

%% RPL_WHOWASUSER
handle_numeric_reply(314, _Msg) ->
    ok;
%% RPL_ENDOFWHOWAS
handle_numeric_reply(369, _Msg) ->
    ok;

%% - Replies RPL_LIST, RPL_LISTEND mark the actual replies
%% with data and end of the server's response to a LIST
%% command.  If there are no channels available to return,
%% only the end reply MUST be sent.

%% RPL_LIST
handle_numeric_reply(322, _Msg) ->
    ok;
%% RPL_LISTEND
handle_numeric_reply(323, _Msg) ->
    ok;
%% RPL_UNIQOPIS
handle_numeric_reply(325, _Msg) ->
    ok;
%% RPL_CHANNELMODEIS
handle_numeric_reply(324, _Msg) ->
    ok;

%% - When sending a TOPIC message to determine the
%% channel topic, one of two replies is sent.  If
%% the topic is set, RPL_TOPIC is sent back else
%% RPL_NOTOPIC.

%% RPL_NOTOPIC
handle_numeric_reply(331, _Msg) ->
    ok;
%% RPL_TOPIC
handle_numeric_reply(332, _Msg) ->
    ok;

%% - Returned by the server to indicate that the
%% attempted INVITE message was successful and is
%% being passed onto the end client.

%% RPL_INVITING
handle_numeric_reply(341, _Msg) ->
    ok;

%% - Returned by a server answering a SUMMON message to
%% indicate that it is summoning that user.

%% RPL_SUMMONING
handle_numeric_reply(342, _Msg) ->
    ok;

%% - When listing the 'invitations masks' for a given channel,
%% a server is required to send the list back using the
%% RPL_INVITELIST and RPL_ENDOFINVITELIST messages.  A
%% separate RPL_INVITELIST is sent for each active mask.
%% After the masks have been listed (or if none present) a
%% RPL_ENDOFINVITELIST MUST be sent.

%% RPL_INVITELIST
handle_numeric_reply(346, _Msg) ->
    ok;
%% RPL_ENDOFINVITELIST
handle_numeric_reply(347, _Msg) ->
    ok;

%% - When listing the 'exception masks' for a given channel,
%% a server is required to send the list back using the
%% RPL_EXCEPTLIST and RPL_ENDOFEXCEPTLIST messages.  A
%% separate RPL_EXCEPTLIST is sent for each active mask.
%% After the masks have been listed (or if none present)
%% a RPL_ENDOFEXCEPTLIST MUST be sent.

%% RPL_EXCEPTLIST
handle_numeric_reply(348, _Msg) ->
    ok;
%% RPL_ENDOFEXCEPTLIST
handle_numeric_reply(349, _Msg) ->
    ok;

%% - Reply by the server showing its version details.
%% The <version> is the version of the software being
%% used (including any patchlevel revisions) and the
%% <debuglevel> is used to indicate if the server is
%% running in "debug mode".

%% The "comments" field may contain any comments about
%% the version or further version details.

%% RPL_VERSION
handle_numeric_reply(351, _Msg) ->
    ok;

%% - The RPL_WHOREPLY and RPL_ENDOFWHO pair are used
%% to answer a WHO message.  The RPL_WHOREPLY is only
%% sent if there is an appropriate match to the WHO
%% query.  If there is a list of parameters supplied
%% with a WHO message, a RPL_ENDOFWHO MUST be sent
%% after processing each list item with <name> being
%% the item.

%% RPL_WHOREPLY
handle_numeric_reply(352, _Msg) ->
    ok;
%% RPL_ENDOFWHO
handle_numeric_reply(315, _Msg) ->
    ok;

%% - To reply to a NAMES message, a reply pair consisting
%% of RPL_NAMREPLY and RPL_ENDOFNAMES is sent by the
%% server back to the client.  If there is no channel
%% found as in the query, then only RPL_ENDOFNAMES is
%% returned.  The exception to this is when a NAMES
%% message is sent with no parameters and all visible
%% channels and contents are sent back in a series of
%% RPL_NAMEREPLY messages with a RPL_ENDOFNAMES to mark
%% the end.        

%% RPL_NAMREPLY
handle_numeric_reply(353, _Msg) ->
    ok;
%% RPL_ENDOFNAMES
handle_numeric_reply(366, _Msg) ->
    ok;

%% - In replying to the LINKS message, a server MUST send
%% replies back using the RPL_LINKS numeric and mark the
%% end of the list using an RPL_ENDOFLINKS reply.

%% RPL_LINKS
handle_numeric_reply(364, _Msg) ->
    ok;
%% RPL_ENDOFLINKS
handle_numeric_reply(365, _Msg) ->
    ok;

%% - When listing the active 'bans' for a given channel,
%% a server is required to send the list back using the
%% RPL_BANLIST and RPL_ENDOFBANLIST messages.  A separate
%% RPL_BANLIST is sent for each active banmask.  After the
%% banmasks have been listed (or if none present) a
%% RPL_ENDOFBANLIST MUST be sent.

%% RPL_BANLIST
handle_numeric_reply(367, _Msg) ->
    ok;
%% RPL_ENDOFBANLIST
handle_numeric_reply(368, _Msg) ->
    ok;

%% - A server responding to an INFO message is required to
%% send all its 'info' in a series of RPL_INFO messages
%% with a RPL_ENDOFINFO reply to indicate the end of the
%% replies.

%% RPL_INFO
handle_numeric_reply(371, _Msg) ->
    ok;
%% RPL_ENDOFINFO
handle_numeric_reply(374, _Msg) ->
    ok;

%% - When responding to the MOTD message and the MOTD file
%% is found, the file is displayed line by line, with
%% each line no longer than 80 characters, using
%% RPL_MOTD format replies.  These MUST be surrounded
%% by a RPL_MOTDSTART (before the RPL_MOTDs) and an
%% RPL_ENDOFMOTD (after).

%% RPL_MOTDSTART
handle_numeric_reply(375, _Msg) ->
    ok;
%% RPL_MOTD
handle_numeric_reply(372, _Msg) ->
    ok;
%% RPL_ENDOFMOTD
handle_numeric_reply(376, _Msg) ->
    gen_server:cast(self(), {send_raw, <<"JOIN #erlounge">>}),
    ok;

%% - RPL_YOUREOPER is sent back to a client which has
%% just successfully issued an OPER message and gained
%% operator status.

%% RPL_YOUREOPER
handle_numeric_reply(381, _Msg) ->
    ok;

%% - If the REHASH option is used and an operator sends
%% a REHASH message, an RPL_REHASHING is sent back to
%% the operator.

%% RPL_REHASHING
handle_numeric_reply(382, _Msg) ->
    ok;

%% - Sent by the server to a service upon successful
%% registration.

%% RPL_YOURESERVICE
handle_numeric_reply(383, _Msg) ->
    ok;

%% - When replying to the TIME message, a server MUST send
%% the reply using the RPL_TIME format below.  The string
%% showing the time need only contain the correct day and
%% time there.  There is no further requirement for the
%% time string.

%% RPL_TIME
handle_numeric_reply(391, _Msg) ->
    ok;

%% - If the USERS message is handled by a server, the
%% replies RPL_USERSTART, RPL_USERS, RPL_ENDOFUSERS and
%% RPL_NOUSERS are used.  RPL_USERSSTART MUST be sent
%% first, following by either a sequence of RPL_USERS
%% or a single RPL_NOUSER.  Following this is
%% RPL_ENDOFUSERS.

%% RPL_USERSSTART
handle_numeric_reply(392, _Msg) ->
    ok;
%% RPL_USERS
handle_numeric_reply(393, _Msg) ->
    ok;
%% RPL_ENDOFUSERS
handle_numeric_reply(394, _Msg) ->
    ok;
%% RPL_NOUSERS
handle_numeric_reply(395, _Msg) ->
    ok;
handle_numeric_reply(_, Msg) ->
    io:format("Unhandled: ~p ~n",[Msg]).
%% 
%% handle_numeric_reply(, Msg) ->
%%     ok;
%% 
