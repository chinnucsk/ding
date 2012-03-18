%%% @author Gert <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert
%%% @doc
%%% Log to file module for girc.
%%% @end
%%% Created : 18 Mar 2012 by Gert <@G3rtm on Twitter>

-module(ltf).
-export([start/1]).


start(Filename) ->
    {ok, File} = file:open(Filename,[append]),
    loop(File).

loop(File) ->
    receive
        {data, Msg} -> file:write(File, Msg),
                       loop(File);
        _ -> loop(File)
    end.

            

