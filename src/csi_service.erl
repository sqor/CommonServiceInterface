%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% Common Service Interface functional part
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------

-module(csi_service).
-behaviour(csi_server).
-include("csi.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2]).

-record(csi_service_state,{}).

-export([stats_start_all/0,
         stats_stop_all/0,
         services/2
        ]).

-export([collect_services_status/1
        ]).

-export([process_foo/2,
         process_too_long/2,
         process_crashing/2]).

% init the global service
init_service(_InitArgs) ->
%    csi:unregister(),
    {ok,#csi_service_state{}}.

% init paralell process
init(_Args,_ServiceState = #csi_service_state{}) ->
    {ok,undefined}.

% terminate parallell process
terminate(Reason,_State) ->
    case Reason of
        normal ->
            ok;
        WAFIT ->
            ?LOGFORMAT(info,
                       "Common Service Interface process terminated for reason ~p",[WAFIT])
    end.

terminate_service(_Reason,_State) ->
    ok.

services(_Args,State) ->
    {[erlang:process_info(X, registered_name) || X <- pg2:get_members(?CSI_SERVICE_PROCESS_GROUP_NAME)],
     State}.

process_foo(_Args,State) ->
    {hello_world,State}.

process_too_long(_Args,State) ->
    timer:sleep(100000),
    {long_job_fininshed,State}.

process_crashing(Args,State) ->
    A = Args - Args,
    {A,State}.

%% ====================================================================
%% Internal functions
%% ====================================================================
stats_start_all() ->
    lists:foreach(fun(Server) ->
                          case Server =:= self() of
                              true ->
                                  ok;
                              _ ->
                                  csi:stats_start(Server)
                          end
                  end,
                  pg2:get_members(?CSI_SERVICE_PROCESS_GROUP_NAME)).

stats_stop_all() ->
    lists:foreach(fun(Server) ->
                          case Server =:= self() of
                              true ->
                                  ok;
                              _ ->
                                  csi:stats_stop(Server)
                          end
                  end,
                  pg2:get_members(?CSI_SERVICE_PROCESS_GROUP_NAME)).

collect_services_status(OwnState) ->
    lists:foldl(fun(Server,Acc) ->
                        Status = case Server =:= self() of
                                     true ->
                                         OwnState;
                                     _ ->
                                         csi:service_status(Server)
                                 end,
                        [{erlang:process_info(Server, registered_name),Status}
                             | Acc]
                end,
                [],
                pg2:get_members(?CSI_SERVICE_PROCESS_GROUP_NAME)).

