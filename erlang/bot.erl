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
    girc:start_link(?MODULE, "irc.freenode.net", 6667, "yflbot").

handle_msg(Msg) ->
    case ircmsg:command(Msg) of
        <<"PRIVMSG">> -> handle_privmsg(Msg);
        <<"PING">> -> ircmsg:create(<<>>,<<"PONG">>,[],ircmsg:tail(Msg));
        <<"002">> -> ircmsg:create(<<>>,<<"JOIN">>,[<<"#yfl">>],<<>>);
        _ -> none
    end.

-spec handle_privmsg(Msg :: ircmsg:ircmsg()) -> ircmsg:ircmsg() | ok.
handle_privmsg(Msg) ->
    io:format("~p~n",[Msg]).

get_http_contents(Url) ->
    {ok, {_, _, Contents}} = httpc:request(Url),
    Contents.

get_xbox_info_for_username(Username) ->
    JsonURL = "http://api.xboxleaders.com/v2/?gamertag="++binary:bin_to_list(Username)++"&format=json",
    {ok, {_, _, RawJson}} = httpc:request(JsonURL),
    Json = mochijson:decode(RawJson),
    {struct, [User]} = Json,
    {"user", {struct, Props}} = User,
    GamerScore = proplists:get_value("gamerscore", Props),
    OnlineStatus = proplists:get_value("online_status", Props),
    io_lib:format("~s (GS: ~p)", [OnlineStatus, GamerScore]).

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
