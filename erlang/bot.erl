%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2011, Gert Meulyzer
%%% @doc
%%% IRC bot using my irc_connection behaviour
%%% @end
%%% Created : 26 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>

-module(bot).

-behaviour(girc).
-include("irc.hrl").

-export([start/0, handle_msg/1, check_for_url/1]).

start() ->
    inets:start(),
    girc:start_link(?MODULE, "irc.freenode.net", 6667, "yflbot").

handle_msg(#ircmsg{command = <<"PRIVMSG">>}=Msg) ->
    handle_privmsg(Msg);
handle_msg(#ircmsg{command = <<"PING">>, tail=T}) ->
    #ircmsg{command="PONG", tail=T};
handle_msg(#ircmsg{command = <<"002">>}) ->
    #ircmsg{command="JOIN", arguments=["#yfl"]};
handle_msg(#ircmsg{prefix=_P, command=_C, arguments=_A, tail=_T}=_Msg) ->
    ok.

-spec handle_privmsg(Msg :: #ircmsg{}) -> #ircmsg{} | ok.
handle_privmsg(#ircmsg{prefix=_P, command=_C, arguments=A, tail=T}=Msg) ->
    io:format("~p~n",[Msg]),
    case A of
        [<<"#euronarp">>] ->
            parse_gt_command(T);
        _ -> ok
    end.

parse_gt_command(Line) ->
    if byte_size(Line) > 2 ->
            case binary:part(Line, 0, 3) of
                <<"!GT">> ->
                    UserName = binary:replace(Line, <<"!GT ">>, <<"">>),
                    Status = (catch get_xbox_info_for_username(UserName)),
                    Reply = case Status of
                                {'EXIT', _} -> "Error!";
                                _ -> Status
                            end,
                    #ircmsg{command="PRIVMSG",
                            arguments=["#euronarp"],
                            tail=Reply};
                _ -> ok
            end;
       true -> ok
    end.

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
