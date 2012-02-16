%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2011, Gert Meulyzer
%%% @doc
%%% IRC bot using my irc_connection behaviour
%%% @end
%%% Created : 26 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>

-module(bot).

-behaviour(girc).

-export([start/0, handle_msg/1, check_for_url/1]).

start() ->
    inets:start(),
    girc:start_link(?MODULE, "irc.freenode.net", 6667, "dingbot").

handle_msg(Msg) ->
    case ircmsg:command(Msg) of
        <<"PRIVMSG">> -> handle_privmsg(Msg);
        <<"PING">> -> ircmsg:create(<<>>,<<"PONG">>,[],ircmsg:tail(Msg));
        <<"002">> -> ircmsg:create(<<>>,<<"JOIN">>,[<<"#erlounge">>],<<>>); %% this is a little crude, it won't work on all IRC servers.
        _ -> none
    end.

-spec handle_privmsg(Msg :: ircmsg:ircmsg()) -> ircmsg:ircmsg() | ok.
handle_privmsg(Msg) ->
    io:format("~p~n",[Msg]).

check_for_url(Line) ->
    Pattern="(http|ftp|https):\\/\\/[\\w\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\.,@?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?",
    {ok, Regex} = re:compile(Pattern,[caseless]),
    case re:run(Line, Regex, [{capture, first, binary}]) of
        {match, [H]} -> H;
        _ -> []
    end.

tinyurl(Url) ->
    %% http://tinyurl.com/api-create.php?url=http://scripting.com/
    undefined.
