%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2011, Gert Meulyzer
%%% @doc
%%% IRC bot using my irc_connection behaviour
%%% @end
%%% Created : 26 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>

-module(bot).

-behaviour(girc).

-export([start/0, stop/0, handle_msg/1, check_for_url/1]).

-export([handle_join/1]).

start() ->
    inets:start(),
    girc:start_link(?MODULE, "irc.freenode.net", 6667, "dingding").

stop() ->
    girc:terminate("Stopping",none).

handle_msg(Msg) ->
    case ircmsg:command(Msg) of
        <<"PRIVMSG">> -> handle_privmsg(Msg);
        <<"PING">> -> ircmsg:create(<<>>,<<"PONG">>,[],ircmsg:tail(Msg));
        <<"JOIN">> -> ?MODULE:handle_join(Msg);
        <<"002">> -> ircmsg:create(<<>>,<<"JOIN">>,[<<"#erlounge">>],<<>>); %% this is a little crude, it won't work on all IRC servers.
        _ -> io:format("Unknown: ~p~n",[Msg])
    end.

-spec handle_privmsg(Msg :: ircmsg:ircmsg()) -> ircmsg:ircmsg() | ok.
handle_privmsg(Msg) ->
    ircmsg:show(Msg).

-spec handle_join(Msg :: ircmsg:ircmsg()) -> ircmsg:ircmsg() | ok.
handle_join(Msg) ->
    Channel = hd(ircmsg:arguments(Msg)),
    Nick = ircmsg:nick(Msg),
    io:format("JOIN to ~p by ~p~n",[Channel,Nick]),
    ok.

check_for_url(Line) ->
    Pattern="(http|ftp|https):\\/\\/[\\w\-_]+(\\.[\\w\\-_]+)+([\\w\\-\\.,@?^=%&amp;:/~\\+#]*[\\w\\-\\@?^=%&amp;/~\\+#])?",
    {ok, Regex} = re:compile(Pattern,[caseless]),
    case re:run(Line, Regex, [{capture, first, binary}]) of
        {match, [H]} -> H;
        _ -> []
    end.

%% tinyurl(Url) ->
%%     %% http://tinyurl.com/api-create.php?url=http://scripting.com/
%%     undefined.
