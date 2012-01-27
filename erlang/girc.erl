%%%-------------------------------------------------------------------
%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2011, Gert Meulyzer
%%% @doc
%%% attempt at IRC client in a gen_server
%%% @end
%%% Created : 28 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(girc).
-compile(export_all).

-behaviour(gen_server).
-include("irc.hrl").
%% API
-export([start_link/4]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
		 terminate/2, code_change/3]).

-export([send_msg/2, send_raw/2]).

-define(SERVER, ?MODULE). 

-record(state, {socket, host, port=6667, username, callbackmodule}).

-callback handle_msg(Message :: #ircmsg{}) -> Reply :: #ircmsg{} | ok.

%%%===================================================================
%%% API
%%%===================================================================

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
	{ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
	{ok, _} = gen_tcp:recv(Sock, 0),
	inet:setopts(Sock, [{active, true}]),
	gen_tcp:send(Sock, "USER "++UserName++" "++UserName++" "++UserName++" "++UserName),
	gen_tcp:send(Sock, "\r\n"),
	gen_tcp:send(Sock, "NICK "++UserName),
	gen_tcp:send(Sock, "\r\n"),
	{ok, #state{socket=Sock, host=Host, port=Port, username=UserName, callbackmodule=Module}}.

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
handle_cast({send_msg, #ircmsg{}=Msg}, #state{socket=Sock}=State) ->
	send_ircmsg(Sock, Msg),
	{noreply, State};
handle_cast(stop, State) ->
	{stop, normal, State};
handle_cast(_Msg, State) ->
	{noreply, State}.

%% wrappers
send_msg(Pid, #ircmsg{}=Msg) ->
	gen_server:cast(Pid, {send_msg, Msg}).
send_raw(Pid, Line) ->
	gen_server:cast(Pid, {send_raw, Line}).

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
handle_info({tcp, _S, Data}, #state{socket=Sock,callbackmodule=Mod}=State) ->
	Lines = lines(Data),
	Msgs = lists:map(fun(X) ->
							 R = (catch parse_line(X)),
							 case R of
								 {'EXIT',_} -> #ircmsg{command = <<"NOOP">>};
								 _ -> R
							 end
					 end, Lines),
	io:format("~p~n", [Msgs]),
	Responses = lists:map(fun Mod:handle_msg/1, Msgs),
	lists:foreach(fun(R) ->
						  case R of
							  ok -> ok;
							  [_|_] -> 
								  lists:foreach(fun(X) -> send_ircmsg(Sock, X) end, R);
							  _ -> send_ircmsg(Sock, R)
						  end
				  end,
				  Responses),
	{noreply, State};
handle_info({tcp_closed, _Port}, State) ->
	{stop, disconnected, State};
handle_info(Info, State) ->
	io:format("UNKNOWN: ~p~n",[Info]),
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

lines(Packet) ->
    lists:filter(fun(X) -> not(X =:= <<>>) end,
                 binary:split(Packet, <<"\r\n">>)).

starts_with_colon(Bin) ->
    case Bin of
        [] -> false;
		<<>> -> false;
        _ -> binary:first(Bin) == 58
    end.

next_word2(Bin) ->
    case Bin of
        [] -> {[], []};
        _ ->
            case binary:split(Bin, <<" ">>) of
                [H] -> {H, []};
                [H|T] -> {H, hd(T)}
            end
    end.

get_words_before_colon(Bin) ->
    case Bin of
        <<>> -> {[], Bin};
        _ -> get_another_word(Bin, [])
    end.

get_another_word(Bin, Words) ->
    case starts_with_colon(Bin) of
        true -> {Words, Bin};
        false ->
            {Word, Rest} = next_word2(Bin),
            case Rest of
                [] -> {[Word|Words], <<>>};
                _ -> get_another_word(Rest, [Word|Words])
            end
    end.

without_colon(Bin) ->
    hd(tl(binary:split(Bin, <<":">>))).

parse_line(IrcBinLine) ->
    case IrcBinLine of
        <<>> -> #ircmsg{};
        _ ->
            {Prefix, CommandsAnd} =
                case starts_with_colon(IrcBinLine) of
                    true ->
                        {P, C} = next_word2(IrcBinLine),
                        {without_colon(P), C};
                    false ->
                        {undefined, IrcBinLine}
                end,
            {Command, ArgsAnd} = next_word2(CommandsAnd),
            {Args, T} = get_words_before_colon(ArgsAnd),
            Tail = case T of
                       <<>> -> <<>>;
                       _ -> without_colon(T)
                   end,
            #ircmsg{prefix=Prefix, command=Command, arguments=Args, tail=Tail}
    end.

send_ircmsg(Sock, #ircmsg{prefix=P,command=C, arguments=A, tail=T}) ->
    case P of
        undefined -> ok;
        _ -> gen_tcp:send(Sock,":"),
             gen_tcp:send(Sock, P),
             gen_tcp:send(Sock, " ")
    end,
    gen_tcp:send(Sock, C),
    gen_tcp:send(Sock, " "),
    case A of
        [] -> ok;
        [[]] -> ok;
        _ -> gen_tcp:send(Sock, string:join(A," ")),
             gen_tcp:send(Sock, " ")
    end,
    case T of
        <<>> -> ok;
        [] -> ok;
        undefined -> ok;
        _ -> gen_tcp:send(Sock, ":"),
             gen_tcp:send(Sock, T)
    end,
    gen_tcp:send(Sock,"\r\n").

send_rawmsg(Sock, Line) ->
	gen_tcp:send(Sock, Line),
	gen_tcp:send(Sock, "\r\n").

test_it() ->
    io:format("~p~n", [parse_line(<<":some.prefix.of.the.server PRIVMSG #testchannel :this is the text">>)]),
    io:format("~p~n", [parse_line(<<"PING :pingeding">>)]),
    io:format("~p~n", [parse_line(<<"COMMAND argument">>)]),
    io:format("~p~n", [parse_line(<<"COMMAND">>)]),
    io:format("~p~n", [parse_line(<<":pre NOTICE #channel :blaa">>)]).

