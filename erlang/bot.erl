%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2011, Gert Meulyzer
%%% @doc
%%% IRC bot using my irc_connection behaviour
%%% @end
%%% Created : 26 Dec 2011 by Gert Meulyzer <@G3rtm on Twitter>

-module(bot).

-behaviour(girc).
-include("irc.hrl").
-compile(export_all).
-export([start/0, handle_msg/1, handle_disconnect/0]).

start() ->
    inets:start(),
    girc:start_link(?MODULE, "irc.freenode.net", 6667, "yflbot").


handle_msg(#ircmsg{prefix=_P, command=C, arguments=_A, tail=T}=Msg) ->
    case C of
        <<"PING">> ->
            io:format("Replying with PONG ~p~n",[T]),
            #ircmsg{command="PONG",
                    tail=T};
        <<"002">> ->
            #ircmsg{command="JOIN", arguments=["#yfl"]};
        <<"PRIVMSG">> ->
            handle_privmsg(Msg);
        _ -> ok
    end.

handle_disconnect() ->
    io:format("DISCONNECTED!~n"),
    irc_connection:connect(?MODULE, "irc.freenode.net", 6667, "yflbot"),
    ok.

-spec handle_privmsg(Msg :: #ircmsg{}) -> #ircmsg{} | ok.
handle_privmsg(#ircmsg{prefix=_P, command=_C, arguments=A, tail=T}=_Msg) ->
    case A of
        [<<"#euronarp">>] ->
            parse_gt_command(T);
        _ -> ok
    end.

parse_gt_command(Line) ->
    if byte_size(Line) > 2 ->
            case binary:part(Line,0,3) of
                <<"!GT">> ->
                    UserName = binary:replace(Line,<<"!GT ">>,<<"">>),
                    Status = (catch get_xbox_info_for_username(UserName)),
                    Reply = case Status of
                                {'EXIT',_} -> "Error!";
                                _ -> Status
                            end, 
                    #ircmsg{command="PRIVMSG",
                            arguments=["#euronarp"],
                            tail=Reply};
                _ -> ok
            end;
       true -> ok
    end.

get_xbox_info_for_username(Username) ->
    JsonURL = "http://api.xboxleaders.com/v2/?gamertag="++binary:bin_to_list(Username)++"&format=json",
    {ok, {_,_,RawJson}} = httpc:request(JsonURL),
    Json = mochijson:decode(RawJson),
    {struct, [User]} = Json,
    {"user",{struct, Props}} = User,
    GamerScore = proplists:get_value("gamerscore", Props),
    OnlineStatus = proplists:get_value("online_status", Props),
    io_lib:format("~s (GS: ~p)", [OnlineStatus, GamerScore]).

    



