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

-export([send_msg/2, send_raw/2, ping_server/1]).

-define(SERVER, ?MODULE).

-record(state, {socket, 
                host, 
                port=6667, 
                username, 
                callbackmodule,
                passthrough=true }).

%%%===================================================================
%%% Behaviour definition
%%%===================================================================
-callback handle_msg(Message :: #ircmsg{}) -> Reply :: #ircmsg{} | ok.


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
    %% helper process to check if we're still connected.
    spawn_link(?MODULE, ping_server, [Sock]),
    %% Do the IRC login
    gen_tcp:send(Sock, "USER "++UserName++" "++UserName++" "++UserName++" "++UserName),
    gen_tcp:send(Sock, "\r\n"),
    gen_tcp:send(Sock, "NICK "++UserName),
    gen_tcp:send(Sock, "\r\n"),
    {noreply, #state{socket=Sock, host=Host, port=Port, username=UserName, callbackmodule=Module}};
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
            ok %% TODO: Parse most of the messages here and create callback functions for them.
    end,
    {noreply, State};
handle_info({tcp_closed, _Port}, State) ->
    io:format("DISCONNECTED!!!!"),
    {stop, disconnected, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
    io:format("TCP ERROR! ~p~n",[Reason]),
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
    gen_tcp:send(Sock, iolist_to_binary([ircmsg:to_line(Msg),<<"\r\n">>])).

send_rawmsg(Sock, Line) ->
    gen_tcp:send(Sock, [Line, "\r\n"]).

ping_server(Sock) ->
    receive
    after 57000 ->
            send_rawmsg(Sock, <<"PING :ConnectionCheck">>),
            ping_server(Sock)
    end.


%%%===================================================================
%%% Main dispatcher
%%%===================================================================

%% -spec handle(#ircmsg{}) -> ok.
%% handle(#ircmsg{prefix=P, command=C, 

%%%===================================================================
%%% Callbacks
%%%===================================================================

