%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2011, Gert Meulyzer
%%% @doc
%%%
%%% @end
%%% Created : 21 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>

-module(irc_connection).

-export([connect/4]).

-include("irc.hrl").
-opaque ircmsg() :: #ircmsg{}.
-export_type([ircmsg/0]).

-callback handle_msg(Message :: ircmsg()) -> Reply :: ircmsg() | ok.

connect(Mod, Host, Port, UserName) ->
    {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 0}, {active, false}]),
	{ok, _} = gen_tcp:recv(Sock, 0),
	inet:setopts(Sock, [{active, true}]),
	gen_tcp:send(Sock, "USER "++UserName++" "++UserName++" "++UserName++" "++UserName),
	gen_tcp:send(Sock, "\r\n"),
	gen_tcp:send(Sock, "NICK "++UserName),
	gen_tcp:send(Sock, "\r\n"),
    loop(Mod, Sock).

loop(Mod, Sock) ->
    receive
        {tcp, _S, Data} ->
            Lines = lines(Data),
%%            Msgs = lists:map(fun parse_line/1, Lines),
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
										  lists:foreach(fun(X) -> send_message(Sock, X) end, R);
                                      _ -> send_message(Sock, R)
                                  end
                          end,
                          Responses),
            loop(Mod, Sock);
        {tcp_closed, _Port} ->
            Mod:handle_disconnect();
        {send, #ircmsg{} = Msg} ->
            send_message(Sock, Msg),
			loop(Mod, Sock);
		{send_raw, Line} ->
			gen_tcp:send(Sock, Line),
			gen_tcp:send(Sock, "\r\n"),
			loop(Mod, Sock);
		{stop, QuitMsg} ->
			send_message(Sock, #ircmsg{command="QUIT", tail=QuitMsg}),
			gen_tcp:close(Sock);
        A ->
            io:format("Unknown: ~p~n", [A]),
            loop(Mod, Sock)
    end.

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

send_message(Sock, #ircmsg{prefix=P,command=C, arguments=A, tail=T}) ->
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

test_it() ->
    io:format("~p~n", [parse_line(<<":some.prefix.of.the.server PRIVMSG #testchannel :this is the text">>)]),
    io:format("~p~n", [parse_line(<<"PING :pingeding">>)]),
    io:format("~p~n", [parse_line(<<"COMMAND argument">>)]),
    io:format("~p~n", [parse_line(<<"COMMAND">>)]),
    io:format("~p~n", [parse_line(<<":pre NOTICE #channel :blaa">>)]).
