%%%-------------------------------------------------------------------
%%% @author Gert <@G3rtm on Twitter>
%%% @copyright (C) 2012, Gert
%%% @doc
%%%
%%% @end
%%% Created :  5 Feb 2012 by Gert <@G3rtm on Twitter>
%%%-------------------------------------------------------------------
-module(ding).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @spec start(StartType, StartArgs) -> {ok, Pid} |
%%                                      {ok, Pid, State} |
%%                                      {error, Reason}
%%      StartType = normal | {takeover, Node} | {failover, Node}
%%      StartArgs = term()
%% @end
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    case ding_sup:start_link() of
        {ok, Pid} ->
            {ok, Pid};
        Error ->
            Error
    end.
    %% Bot = {dingding, {'bot', start_link, [dingding]},
    %%        permanent, 2000, worker, ['bot']},
    %% Backend = {backend, {girc, start_link, ["irc.freenode.net",6667, "dingding", dingding]},
    %%            permanent, 2000, worker, [girc]}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @spec stop(State) -> void()
%% @end
%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


