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

-export([service_status/2,
         stats_start/2,
         stats_stop/2
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

service_status(_Args,State) ->
    {lists:foldl(fun(Server,Acc) ->
                         Status = case Server =:= self() of
                                      true ->
                                          State;
                                      _ ->
                                          csi:service_status(Server)
                                  end,
                         [{erlang:process_info(Server, registered_name),Status}
                              | Acc]
                 end,
                 [],
                 pg2:get_members(?CSI_SERVICE_PROCESS_GROUP_NAME)),
     State}.

stats_start(_Args,State) ->
    lists:foreach(fun(Server) ->
                          case Server =:= self() of
                              true ->
                                  csi:cast(Server,'$stats_start');
                              _ ->
                                  csi:stats_start(Server)
                          end
                  end,
                  pg2:get_members(?CSI_SERVICE_PROCESS_GROUP_NAME)),
    {ok,State}.

stats_stop(_Args,State) ->
    lists:foreach(fun(Server) ->
                          case Server =:= self() of
                              true ->
                                  csi:cast(Server,'$stats_stop');
                              _ ->
                                  csi:stats_stop(Server)
                          end
                  end,
                  pg2:get_members(?CSI_SERVICE_PROCESS_GROUP_NAME)),
    {ok,State}.

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

