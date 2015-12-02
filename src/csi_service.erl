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
-include("csi_common.hrl").
-include("csi.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2,
         handle_call/3]).

-record(csi_service_state, {}).
-record(csi_request_state, {}).

-export([stats_start_all/0,
         stats_stop_all/0,
         services/2,
         start_services/2
        ]).

-export([collect_services_status/1
        ]).

-export([process_foo/2,
         process_too_long/2,
         process_crashing/2]).

% init the global service
init_service(_InitArgs) ->
    csi:cast(?CSI_SERVICE_NAME, start_services, []),
    {ok, #csi_service_state{}}.

% init paralell process
init(_Args, _ServiceState = #csi_service_state{}) ->
    {ok, #csi_request_state{}}.

% terminate parallell process
terminate(Reason, _State) ->
    case Reason of
        normal ->
            ok;
        WAFIT ->
            ?LOGFORMAT(info,
                       "Common Service Interface process terminated "
                       "for reason ~p", [WAFIT])
    end.

terminate_service(_Reason, _State) ->
    normal.

start_services(_Args, State) ->
    ServerList =
        case application:get_env(?CSI_APPLICATION_NAME, servers) of
            {ok, Value} ->
                Value;
            undefined ->
                []
        end,
    {start_servers(ServerList),State}.
    
services(_Args, State) ->
    {[erlang:process_info(X, registered_name) ||
        X <- pg2:get_members(?CSI_SERVICE_PROCESS_GROUP_NAME)],
     State}.

handle_call({Request, Args}, _From, State) ->
    ?LOGFORMAT(info,"Calling service through handle_call(~p)",[{Request, Args}]),
    {Reply, NewState} = ?MODULE:Request(Args, State),
    {reply, Reply, NewState};

handle_call(Request, _From, State) ->
    ?LOGFORMAT(warning, "Unhandled request:~p for csi_service with state:~p~n",
               [Request, State]),
    {reply, ok, State}.

process_foo(_Args, State) ->
    {hello_world, State}.

process_too_long(_Args, State) ->
    timer:sleep(5000),
    {long_job_finished, State}.

process_crashing(Args, State) ->
    A = Args - Args,
    {A, State}.

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
    lists:foldl(fun(Server, Acc) ->
                        Status = case Server =:= self() of
                                     true ->
                                         OwnState;
                                     _ ->
                                         csi:service_status(Server)
                                 end,
                        [{erlang:process_info(Server, registered_name), Status}
                             | Acc]
                end,
                [],
                pg2:get_members(?CSI_SERVICE_PROCESS_GROUP_NAME)).

start_servers([]) ->
    ok;

start_servers([{Name, Module, InitArgs, ChildSpec} | Tail]) ->
    Child = case ChildSpec of
                default ->
                    #{id => Name,
                      start => {?CSI_APPLICATION_NAME,
                                start_link,
                                [Name, Module,InitArgs]},
                      restart => permanent,
                      shutdown => 2000,
                      type => worker,
                      modules => []};
                _ ->
                    ChildSpec
            end,
    case supervisor:start_child(csi_sup, Child) of
        {ok, _WorkerPId} ->
            ?LOGFORMAT(info,"Service ~p started by CSI.",[Name]);
        WAFIT ->
            ?LOGFORMAT(error,"CSI could not start service:~p. Reason:~p",[Name,WAFIT])
    end,
    start_servers(Tail).
