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
-define(COLON, 58).

%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-export([send_msg/2, send_raw/2, get_name/1]).

-define(SERVER, ?MODULE).

-record(state, {socket, host, port=6667, username, name}).

-callback handle_msg(Message :: ircmsg:ircmsg()) -> Reply :: ircmsg:ircmsg() | ok.

%%%===================================================================
%%% API
%%%===================================================================

%% -spec send_msg(Pid :: pid()) -> Msg :: ircmsg:ircmsg().
send_msg(Pid, Msg) ->
    gen_server:cast(Pid, {send_msg, Msg}).
send_raw(Pid, Line) ->
    gen_server:cast(Pid, {send_raw, Line}).

-spec get_name(atom()) -> atom().
get_name(Name) ->
    Str = atom_to_list(Name) ++ "backend",
    list_to_atom(Str).
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Host, Port, UserName, Name) ->
    gen_server:start_link({local, get_name(Name)}, ?MODULE, [Host, Port, UserName, Name], []).

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
init([Host, Port, UserName, Name]) ->
    {ok, #state{host=Host, port=Port, username=UserName, name=Name}, 0}.

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
handle_info(timeout, #state{host=Host, port=Port, username=UserName}) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, line}, {active, true}]),
    %% Do the IRC login
    gen_tcp:send(Sock, "USER "++UserName++" "++UserName++" "++UserName++" "++UserName),
    gen_tcp:send(Sock, "\r\n"),
    gen_tcp:send(Sock, "NICK "++UserName),
    gen_tcp:send(Sock, "\r\n"),
    {noreply, #state{socket=Sock, host=Host, port=Port, username=UserName}};
handle_info({tcp, _S, Data}, #state{name=Name}=State) ->
    Msg = ircmsg:parse_line(Data),
    gen_event:notify(Name, {ircmsg, irmsg:prefix(Msg), ircmsg:command(Msg), ircmsg:arguments(Msg), ircmsg:tail(Msg)}),
    {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
    io:format("DISCONNECTED!!!!"),
    %% we really should do something here to reconnect?
    %% or let the supervisor do that later?
    {stop, disconnected, State};
handle_info(Info, State) ->
    io:format("girc2UNKNOWN: ~p~n", [Info]),
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
    gen_tcp:send(Sock, iolist_to_binary([ircmsg:to_line(Msg),<<"\r\n">>])).

send_rawmsg(Sock, Line) ->
    gen_tcp:send(Sock, [Line, "\r\n"]).


