%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @TODO update the doc here as it reflects an earlier version.
%%% @doc Generic server for making request processing in parallell
%%% Usage:  For a service, create a file service_name.erl
%%%         and service_name_service_module.erl.
%%%         For example the relationship service shall have rls.erl and
%%%         rls_service.erl
%%%
%%% service_name.erl is the api for that service. It shall contain at least
%%% the following exported functions that is recommended to define as follows:
%%% start() -> cu_pserver:start(?YOUR_REGISTERED_SERVICE_NAME,
%%%                             service_name>_service_module).
%%% start_link() -> cu_pserver:start_link(?YOUR_REGISTERED_SERVICE_NAME,
%%%                                       service_name_service_module).
%%% stop() -> cu_pserver:stop(?YOUR_REGISTERED_SERVICE_NAME).
%%% 
%%% Thus the supervisor of your service can restart your gen_server.
%%%
%%% In addition you shall define your service API in the following way:
%%% service_function(Arguments) ->
%%%     cu_pserver:call(?YOUR_REGISTERED_SERVICE_NAME,
%%%                     service_function,
%%%                     Arguments).
%%% 
%%% When a call arrives to your api, it sends the request to the locally
%%% registered gen_server for your service. It then tries to call the function
%%% in your service_name_service_module withing a try - cathc.
%%% This gen_server then spawns a separate process to handle the incoming
%%% request and get ready for the next request.
%%% In case this server is down and being relaunched by its supervisor, after
%%% ?DEFAULT_SERVICE_SLEEP time, this cu_pserver module will retry it for
%%% ?DEFAULT_SERVICE_RETRY times. If that fails, it returns an {error, noserver}
%%% tuple to the caller.
%%% 
%%% If everything goes fine, your function with the argument is called in a
%%% separate thread and when that returns, the return value is sent back to the
%%% caller.
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------
-module(csi_server).

-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).

-define(LOGFORMAT(Severity,Msg,Args),lager:Severity(Msg,Args)).

-include("csi.hrl").

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-callback init_service(InitArgs :: term()) -> Result :: term().
-callback init(InitArgs :: term(), State :: term()) -> Result :: term().
-callback terminate(Reason :: term(), State :: term()) -> Result :: term().
-callback terminate_service(Reason :: term(), State :: term()) -> Result :: term().

%% ====================================================================
%% API functions
%% ====================================================================
%% The number of times a gen server will be called in case it is down
%% as waiting for the supervisor to relaunch it
-export([process_service_request/8
        ]).

-export([response_time/6
        ]).


%% ====================================================================
%% Behavioural functions 
%% ====================================================================
-record(rstate, {service_name,
                 service_module,
                 service_state,
                 stats_collect = true,
                 stat_table,
                 stats_process_table,
                 stats_module = ?MODULE,
                 stats_requests_include = [all],
                 stats_requests_exclude = [],
%                 stats_types = [response_time]
                 stats_types = [{response_time,[{"last_nth_to_collect",10},
                                                {"normalize_to_nth",8}]}
%%                                ,{req_per_sec,[{"time_window",5}
%%                                              ]}
                               ]
                }).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">
%% gen_server:init/1</a>
%% @end
-spec init({Name :: atom(), Module :: atom(), InitArgs :: term()}) ->
          Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init({Name, Module, InitArgs}) ->
    process_flag(trap_exit, true),
    StatTable = ets:new(Name, [private]),
    ProcTable = ets:new(list_to_atom(atom_to_list(Name) ++ "_procs"),[private]),
    pg2:create(?CSI_SERVICE_PROCESS_GROUP_NAME),
    csi:register(),
    try case Module:init_service(InitArgs) of
            {ok,SState} ->
                {ok,#rstate{service_name = Name,
                            service_module = Module,
                            service_state = SState,
                            stat_table = StatTable,
                            stats_process_table = ProcTable}};
            WAFIT ->
                {stop,WAFIT}
        end
    catch
        A:B ->
            ?LOGFORMAT(error,
                       "Exception when calling init_service for service ~p as"
                       "~p:init_service(~p). ~p:~p. Stacktrace:~p",
                       [Name,
                        Module,
                        InitArgs,
                        A,
                        B,
                        erlang:get_stacktrace()]),
            {stop,exception}
    end.
    
%% handle_call/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:\
%% handle_call-3">gen_server:handle_call/3</a>
%% @end
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: term()) ->
          Result when
    Result :: {reply, Reply, NewState}
            | {reply, Reply, NewState, Timeout}
            | {reply, Reply, NewState, hibernate}
            | {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason, Reply, NewState}
            | {stop, Reason, NewState},
    Reply :: term(),
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity,
    Reason :: term().
%% ====================================================================


handle_call(stop,_From,State) ->
    {stop,normal,ok,State};

handle_call('$service_status',_From,State) ->
    {reply,State,State};

handle_call('$stats_start',_From,State) ->
    NewState = State#rstate{stats_collect = true},
    {reply,ok,NewState};

handle_call('$stats_stop',_From,State) ->
    NewState = State#rstate{stats_collect = false},
    {reply,ok,NewState};

handle_call({'$stats_include_funs',FunctionList}, From, State)
  when is_atom(FunctionList) ->
    handle_call({'$stats_include_funs',[FunctionList]}, From, State);

handle_call({'$stats_include_funs',FunctionList},_From,State) ->
    % first remove the functions from the exclude list
    ExcludeList = csi_utils:remove_elems_from_list(FunctionList,
                                                   State#rstate.stats_requests_exclude),
    % second include it in stats requests if it does not contain all
    NewState = case lists:member(all, State#rstate.stats_requests_include) of
                   true ->
                       State#rstate{stats_requests_exclude = ExcludeList};
                   _ ->
                       % add to the list of requests if it is not there
                       IncludeList = csi_utils:add_elems_to_list(FunctionList,
                                                                 State#rstate.stats_requests_include),
                       State#rstate{stats_requests_exclude = ExcludeList,
                                    stats_requests_include = IncludeList}
               end,
    {reply,ok,NewState};

handle_call({'$stats_exclude_funs',FunctionList}, From, State)
  when is_atom(FunctionList) ->
    handle_call({'$stats_exclude_funs',[FunctionList]}, From, State);

handle_call({'$stats_exclude_funs',FunctionList},_From,State) ->
    % first, remove it from the requests
    IncludeList = csi_utils:remove_elems_from_list(FunctionList,
                                                   State#rstate.stats_requests_include),
    % second, add it to the exclude list
    ExcludeList = csi_utils:add_elems_to_list(FunctionList,
                                              State#rstate.stats_requests_exclude),
    NewState = State#rstate{stats_requests_exclude = ExcludeList,
                         stats_requests_include = IncludeList},
    {reply,ok,NewState};

handle_call({'$stats_set_funs',FunctionList}, From, State)
  when is_atom(FunctionList) ->
    handle_call({'$stats_set',[FunctionList]}, From, State);

handle_call({'$stats_set_funs',FunctionList},_From,State) ->
    NewState = State#rstate{stats_requests_include = FunctionList,
                            stats_requests_exclude = []},
    {reply,ok,NewState};

handle_call('$stats_get_all',_From,State) ->
%%     Reply = lists:flatten(ets:foldl(fun (X,AccIn) ->
%%                                              [io_lib:format("~p~n", [X])|AccIn]
%%                                     end, [], State#rstate.stat_table)),
    Reply = ets:tab2list(State#rstate.stat_table),
    {reply,Reply,State};

handle_call({'$stats_get_funs',FunctionList},_From,State)
    when is_atom(FunctionList) ->
        handle_call({'$stats_get_funs',[FunctionList]},_From,State);

handle_call({'$stats_get_funs',_FunctionList},_From,State) ->
    %% @TODO return the stats for only the functions in the Functionlist
    Reply = ets:tab2list(State#rstate.stat_table),
    {reply,Reply,State};

handle_call({'$stats_get_types',TypeList},_From,State)
    when is_atom(TypeList) ->
        handle_call({'$stats_get_types',[TypeList]},_From,State);

handle_call({'$stats_get_types',_TypeList},_From,State) ->
    %% @TODO return the stats for only the types in the Typelist
    Reply = ets:tab2list(State#rstate.stat_table),
    {reply,Reply,State};

handle_call({'$stats_get_specific',_Function,_Type},_From,State) ->
    %% @TODO return the stats for only the function for a type
    Reply = ets:tab2list(State#rstate.stat_table),
    {reply,Reply,State};

handle_call('$stats_get_process_table',_From,State) ->
    Reply = ets:tab2list(State#rstate.stats_process_table),
    {reply,Reply,State};

handle_call({'$stats_include_type',TypeList}, From, State)
  when is_atom(TypeList) ->
    handle_call({'$stats_include_type',[TypeList]}, From, State);

handle_call({'$stats_include_type',TypeList},_From,State) ->
    IncludeList = csi_utils:add_elems_to_list(TypeList,
                                              State#rstate.stats_types),
    NewState = State#rstate{stats_types = IncludeList},
    {reply,ok,NewState};

handle_call({'$stats_exclude_type',TypeList}, From, State)
  when is_atom(TypeList) ->
    handle_call({'$stats_exclude_type',[TypeList]}, From, State);

handle_call({'$stats_exclude_type',TypeList},_From,State) ->
    % first, remove it from the requests
    ExcludeList = csi_utils:remove_elems_from_list(TypeList,
                                                   State#rstate.stats_types),
    NewState = State#rstate{stats_types = ExcludeList},
    {reply,ok,NewState};

handle_call({'$stats_change_module',Module},_From,State) ->
    %% @TODO check if the module is loaded and load it if not

    NewState = State#rstate{stats_module = Module},
    {reply,ok,NewState};

handle_call({call_p,Request,Args,TimeoutForProcessing} = R,From,State) ->
    collect_stats(start,State,Request,R,Ref = make_ref()),
    Pid = proc_lib:spawn_link(?MODULE,
                              process_service_request,
                              [From,State#rstate.service_module,
                               Request,Args,State,
                               TimeoutForProcessing,self(),true]),
    ets:insert(State#rstate.stats_process_table, {Pid,Ref,Request,R}),
    {noreply, State};

handle_call({post_p,Request, Args,TimeoutForProcessing} = R,From,State) ->
    collect_stats(start,State,Request,R,Ref = make_ref()),
    Pid = proc_lib:spawn_link(?MODULE,
                              process_service_request,
                              [From,State#rstate.service_module,
                               Request,Args,State,
                               TimeoutForProcessing,self(),true]),
    ets:insert(State#rstate.stats_process_table, {Pid,Ref,Request,R}),
    {reply, {posted,{Pid,From}}, State};

handle_call({cast_p,Request, Args,TimeoutForProcessing} = R,From,State) ->
    collect_stats(start,State,Request,R,Ref = make_ref()),
    Pid = proc_lib:spawn_link(?MODULE,
                              process_service_request,
                              [From,State#rstate.service_module,
                               Request,Args,State,
                               TimeoutForProcessing,self(),false]),
    ets:insert(State#rstate.stats_process_table, {Pid,Ref,Request,R}),
    {reply, {casted,{Pid,From}} , State};

handle_call({call_s,Request,Args} = R,_From,State) ->
    collect_stats(start,State,Request,R,Ref = make_ref()),
    Module = State#rstate.service_module,
    try case Module:init(Args,State#rstate.service_state) of
            {ok,PState} ->
                {Reply,EState} = Module:Request(Args,PState),
                Module:terminate(normal,EState),
                collect_stats(stop,State,Request,R,Ref),
                {reply,Reply,State};
            WAFIT ->
               collect_stats(clean,State,Request,R,Ref),
               {reply,WAFIT,State}
        end
    catch
        A:B ->
            ?LOGFORMAT(error,
                       "Exception in service when calling ~p:~p(~p)."
                       " ~p:~p. Stacktrace:~p~n",
                       [Module,
                        Request,
                        Args,
                        A,
                        B,
                        erlang:get_stacktrace()]),
            collect_stats(clean,State,Request,R,Ref),
            {reply,{error,exception},State}
    end;

handle_call(Request, From, State) ->
%    collect_stats(start,State,Request,Request,Ref = make_ref()),
    Module = State#rstate.service_module,
    case Module:handle_call(Request,From,State#rstate.service_state) of
        {reply, Reply, NewState} -> {reply, Reply, State#rstate{service_state = NewState}};
        {reply, Reply, NewState, Timeout} -> {reply, Reply, State#rstate{service_state = NewState},Timeout};
        {noreply, NewState} -> {noreply, State#rstate{service_state = NewState}};
        {noreply, NewState, Timeout} -> {noreply, State#rstate{service_state = NewState},Timeout};
        {stop, Reason, Reply, NewState} -> {stop, Reason, Reply, State#rstate{service_state = NewState}};
        {stop, Reason, NewState} -> {stop, Reason, State#rstate{service_state = NewState}}
    end.

process_service_request(From,Module,Request,Args,State,
                             TimeoutForProcessing,Parent,NeedReply) ->
    TRef = case TimeoutForProcessing of
               infinity ->
                   undefined;
               RealTimeout ->
                   KillMessage = case NeedReply of
                                     true ->
                                         {kill_worker_reply,self(),From};
                                     false ->
                                         {kill_worker_noreply,self()}
                                 end,
                   erlang:send_after(RealTimeout,
                                     Parent,
                                     KillMessage)
           end,
    try case Module:init(Args,State#rstate.service_state) of
            {ok,PState} ->
                {Reply,EState} = Module:Request(Args,PState),
                case NeedReply of
                    true ->
                        gen_server:reply(From,Reply);
                    _ ->
                        ok
                end,
                Module:terminate(normal,EState);
            WAFIT ->
                gen_server:reply(From,WAFIT)
        end
    catch
        A:B ->
            ?LOGFORMAT(error,
                       "Exception in service when calling ~p:~p(~p)."
                       " ~p:~p. Stacktrace:~p~n",
                       [Module,
                        Request,
                        Args,
                        A,
                        B,
                        erlang:get_stacktrace()]),
            catch gen_server:reply(From,{error,exception}),
            erlang:exit(exception)
    end,
    case TRef of
        undefined ->
            ok;
        RealTRef ->
            erlang:cancel_timer(RealTRef)
    end.

%% handle_cast/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:
%% handle_cast-2">gen_server:handle_cast/2</a>
%% @end
-spec handle_cast(Request :: term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_cast({cast,Request}, State) ->
    handle_call(Request, self(), State),
    {noreply, State};


handle_cast(_Msg, State) ->
    {noreply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:
%% handle_info-2">gen_server:handle_info/2</a>
%% @end
-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
    Result :: {noreply, NewState}
            | {noreply, NewState, Timeout}
            | {noreply, NewState, hibernate}
            | {stop, Reason :: term(), NewState},
    NewState :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
handle_info(Info, State) ->
    case try (State#rstate.service_module):handle_info(Info,State#rstate.service_state)
         catch _:_ -> continue end of
        continue ->
            case Info of
                {'EXIT',Pid,Reason} ->
                    case ets:lookup(State#rstate.stats_process_table, Pid) of
                        [{Pid,Ref,Request,R}] ->
                            case Reason =:= normal of
                                true ->
                                    collect_stats(stop,State,Request,R,Ref);
                                _ ->
                                    ok
                            end,
                            ets:delete(State#rstate.stats_process_table, Pid),
                            collect_stats(clean, State, undefined, undefined, Ref);
                        WAFIT ->
                            ?LOGFORMAT(warning,
                                       "Pid ~p not returned value ~p from process table~n",
                                       [Pid,WAFIT])
                    end;
                {kill_worker_reply,Pid,CallerPid} ->
                    catch gen_server:reply(CallerPid, {error,timeout_killed}),
                    erlang:exit(Pid, kill);
                {kill_worker_noreply,Pid} ->
                    erlang:exit(Pid, kill);
                WAFIT -> 
                    ?LOGFORMAT(warning,
                               "Unhandled message received for service ~p."
                               "Module to be called:~p. Message:~p",
                               [State#rstate.service_name,
                                State#rstate.service_module,
                                WAFIT])
            end,
            {noreply, State};
        Else ->
            Else
    end.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:
%% terminate-2">gen_server:terminate/2</a>
%% @end
-spec terminate(Reason, State :: term()) -> Any :: term() when
    Reason :: normal
            | shutdown
            | {shutdown, term()}
            | term().
%% ====================================================================
terminate(Reason, #rstate{service_module = Module,
                          service_name = Name,
                          service_state = ServiceState,
                          stat_table = StatTable}) ->
    ets:delete(StatTable),
    try Module:terminate_service(Reason,ServiceState)
    catch
        A:B ->
            ?LOGFORMAT(error,
                       "Exception when calling terminate_service for"
                       " service ~p as"
                       "~p:terminate_service(~p,~p). ~p:~p. Stacktrace:~p",
                       [Name,
                        Module,
                        Reason,
                        ServiceState,
                        A,
                        B,
                        erlang:get_stacktrace()]),
            {error,exception}
    end.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#
%% Module:code_change-3">gen_server:code_change/3</a>
%% @end
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
    Result :: {ok, NewState :: term()} | {error, Reason :: term()},
    OldVsn :: Vsn | {down, Vsn},
    Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

collect_stats(Stage,State,Request,R,Ref) ->
    case State#rstate.stats_collect of
        true ->
            case stats_to_collect(Request,State) of
                true ->
                    try
                        lists:map(fun ({M,Params}) ->
                                           (State#rstate.stats_module):
                                           M(Stage,
                                             Request,
                                             R,
                                             Ref,
                                             Params,
                                             State#rstate.stat_table)
                                  end,
                                  State#rstate.stats_types)
                    catch
                        A:B ->
                            ?LOGFORMAT(error,
                                       "Exception when calling "
                                       "~p:~p(~p). ~p:~p --> Stacktrace:~p",
                                       [State#rstate.stats_module,
                                        State#rstate.stats_types,
                                        R,
                                        A,
                                        B,
                                        erlang:get_stacktrace()])
                    end;
                _ ->
                    ok
            end;
        _ ->
            ok
    end.

stats_to_collect(Request,State) ->
    case lists:member(all,State#rstate.stats_requests_include) of
        true ->
            not lists:member(Request,State#rstate.stats_requests_exclude);
        _ ->
            lists:member(Request, State#rstate.stats_requests_include) and
                not lists:member(Request,State#rstate.stats_requests_exclude)
    end.
            
response_time(start,_Request,_R,Ref,_Params,Tab) ->
    ets:insert(Tab, {{response_time_first,Ref},csi_utils:now_usec()});

response_time(stop,Request,_R,Ref,Params,Tab) ->
    case ets:lookup(Tab, {response_time_first,Ref}) of
        [{{response_time_first,Ref},Value}] ->
            ResponseTime = csi_utils:now_usec() - Value,
            ets:delete(Tab,{response_time_first,Ref}),
            {NrOfReqs,ART,_Avg,MinRt,MaxRt} = case ets:lookup(Tab, {response_time,Request}) of
                                                 [] ->
                                                     {0,0,0,1000000000000,0};
                                                 [{{response_time,Request},Values}] ->
                                                     Values
                                             end,
            {NormalizedART,NewNr} = 
                case proplists:get_value("last_nth_to_collect",
                                         Params, 
                                         10) =:= NrOfReqs of
                    true ->
                        NormalizeToNth = proplists:get_value("normalize_to_nth",
                                                             Params,
                                                             8),
                        {( ART / NrOfReqs ) * NormalizeToNth,NormalizeToNth + 1};
                    _ ->
                        {ART,NrOfReqs + 1}
                end,
            AllRespTime = NormalizedART + ResponseTime,
            NewMinRt = min(MinRt,ResponseTime),
            NewMaxRt = max(MaxRt,ResponseTime),
            ets:insert(Tab, {{response_time,Request},{NewNr,
                                                      AllRespTime,
                                                      AllRespTime/NewNr,
                                                      NewMinRt,
                                                      NewMaxRt}})
    end;

response_time(clean,_Request,_R,Ref,_Params,Tab) ->
    case Ref of
        undefined ->
            ok;
        Else ->
            ets:delete(Tab, {response_time_first,Else})
    end.

