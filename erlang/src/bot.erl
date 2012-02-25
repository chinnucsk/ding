%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2011, Gert Meulyzer
%%% @doc
%%% IRC bot using my girc behaviour
%%% @end
%%% Created : 26 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>

-module(bot).

-behaviour(girc).

-export([start/0, stop/0, handle_msg/1, check_for_url/1]).

-export([handle/5]).

start() ->
    inets:start(), 
    girc:start_link(?MODULE, "irc.freenode.net", 6667, "dingding").

stop() ->
    girc:terminate("Stopping", none).

-spec handle_msg(ircmsg:irmsg()) -> ircmsg:msg() | ok.
handle_msg(Msg) ->
    Prefix = ircmsg:prefix(Msg), 
    Command = ircmsg:command(Msg), 
    Arguments = ircmsg:arguments(Msg),    
    Tail = ircmsg:tail(Msg), 
    handle(Prefix, Command, Arguments, Tail, Msg).

-spec handle(binary(), binary(), binary(), binary(), ircmsg:ircmsg()) -> ircmsg:ircmsg() | ok.
handle(_, <<"PRIVMSG">>, _, _, Msg) ->
    handle_privmsg(Msg);
handle(_, <<"PING">>, _, Tail, _) ->
    ircmsg:create(<<>>, <<"PONG">>, [], Tail);
handle(P, <<"JOIN">>, A, _, _) ->
    io:format("==JOIN==(~p)=> ~p~n",
              [hd(A),ircmsg:nick_from_prefix(P)]);
handle(P, <<"QUIT">>, _, _, _) ->
    io:format("<==QUIT== ~p~n", [ircmsg:nick_from_prefix(P)]);
handle(_, <<"002">>, _, _, _) ->
    %% this is a little crude, it won't work on all IRC servers.
    %% and it's not configurable, needs to be reworked.
    ircmsg:create(<<>>, <<"JOIN">>, [<<"#erlounge">>], <<>>);
handle(_, _, _, _, Msg) ->
    io:format("Unknown: ~p~n", [Msg]).


-spec handle_privmsg(Msg :: ircmsg:ircmsg()) -> ircmsg:ircmsg() | ok.
handle_privmsg(Msg) ->
    ircmsg:show(Msg).

check_for_url(Line) ->
    Pattern="(http|ftp|https):\\/\\/[\\w\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\., @?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?", 
    {ok, Regex} = re:compile(Pattern, [caseless]), 
    case re:run(Line, Regex, [{capture, first, binary}]) of
        {match, [H]} -> H;
        _ -> []
    end.

%% tinyurl(Url) ->
%%     %% http://tinyurl.com/api-create.php?url=http://scripting.com/
%%     undefined.
