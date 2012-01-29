%%% @author Gert Meulyzer <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert Meulyzer
%%% @doc
%%% Parser module for IRC messages.
%%% This time, let's try to keep everything working with binaries.
%%% -spec Function(ArgName1 :: Type1, ..., ArgNameN :: TypeN) -> RT.
%%% -spec Function(ArgType1, ..., ArgTypeN) -> ReturnType.
%%% A little documentation can't hurt.
%%% @end
%%% Created : 29 Jan 2012 by Gert Meulyzer <@G3rtm on Twitter>

-module(parser).
-include("irc.hrl").

-define(COLON, 58).

%% Shut up! This is just here until I finish the module. :p
-compile(export_all).

-spec lines(Packet :: binary()) -> [binary()].
lines(Packet) ->
    binary:split(Packet, <<"\r\n">>, [global, trim]).

-spec starts_with_colon(B :: binary()) -> boolean().
starts_with_colon(<<>>) ->
    false;
starts_with_colon(Bin) ->
    binary:first(Bin) == ?COLON.

