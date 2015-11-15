%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% Common Service Interface gen_server implementation
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------
-module(csi_server).

-behaviour(gen_server).

-include("csi_common.hrl").
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
-callback terminate_service(Reason :: term(),
                            State :: term()) -> Result :: term().

%% ====================================================================
%% API functions
%% ====================================================================

-export([process_service_request/8
        ]).

-record(csi_service_state, {service_name,
                            service_module,
                            service_state,
                            stats_collect = true,
                            stats_table,
                            stats_temp_table,
                            stats_process_table,
                            stats_module = ?DEFAULT_STATS_MODULE,
                            stats_requests_include = [all],
                            stats_requests_exclude = [],
                            stats_types = [],
                            service_timeout = ?DEFAULT_SERVICE_TIMEOUT
                           }).

-type csi_service_state() :: #csi_service_state{}.

-export_type([csi_service_state/0]).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_server.html#Module:init-1">
%% gen_server:init/1</a>
%% @end
-spec init({Name :: atom(),
            Module :: atom(),
            InitArgs :: term(),
            Options :: property_list()}) ->
          Result when
    Result :: {ok, State}
            | {ok, State, Timeout}
            | {ok, State, hibernate}
            | {stop, Reason :: term()}
            | ignore,
    State :: term(),
    Timeout :: non_neg_integer() | infinity.
%% ====================================================================
init({Name, Module, InitArgs, Options}) ->
    process_flag(trap_exit, true),
    StatTable = ets:new(Name, [private]),
    StatTempTable = ets:new(list_to_atom(atom_to_list(Name) ++ "_temp"),
                            [private]),
    ProcTable = ets:new(list_to_atom(atom_to_list(Name) ++ "_procs"),
                        [private]),
    pg2:create(?CSI_SERVICE_PROCESS_GROUP_NAME),
    ok = csi:register(),
    try case Module:init_service(InitArgs) of
            {ok, SState} ->
                {ok, #csi_service_state{service_name = Name,
                                        service_module = Module,
                                        service_state = SState,
                                        stats_types =
                                            ?DEFAULT_STATS_MODULE:init_stats(),
                                        stats_table = StatTable,
                                        stats_temp_table = StatTempTable,
                                        stats_process_table = ProcTable,
                                        service_timeout =
                                            proplists:get_value(
                                              service_timeout,
                                              Options,
                                              ?DEFAULT_SERVICE_TIMEOUT)}};
            WAFIT ->
                {stop, WAFIT}
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
            {stop, exception}
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


handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call({'$set_options', Options}, _From, State) ->
    NewState =
        lists:foldl(
          fun({Param, Value}, AccIn) ->
                  case Param of
                      service_timeout ->
                          AccIn#csi_service_state{service_timeout = Value};
                      _ ->
                          AccIn
                  end
          end, State, Options),
    {reply, NewState, NewState};

handle_call('$collect_services_status', _From, State) ->
    {reply, csi_service:collect_services_status(State), State};

handle_call('$service_status', _From, State) ->
    {reply, State, State};

handle_call('$stats_start_all', From, State) ->
    csi_service:stats_start_all(),
    handle_call('$stats_start', From, State);

handle_call('$stats_start', _From, State) ->
    NewState = State#csi_service_state{stats_collect = true},
    {reply, ok, NewState};

handle_call('$stats_stop', _From, State) ->
    NewState = State#csi_service_state{stats_collect = false},
    {reply, ok, NewState};

handle_call('$stats_stop_all', From, State) ->
    csi_service:stats_stop_all(),
    handle_call('$stats_stop', From, State);

handle_call({'$stats_include_funs', FunctionList}, From, State)
  when is_atom(FunctionList) ->
    handle_call({'$stats_include_funs', [FunctionList]}, From, State);

handle_call({'$stats_include_funs', FunctionList}, _From, State) ->
    % first remove the functions from the exclude list
    ExcludeList =
        csi_utils:remove_elems_from_list(
          FunctionList,
          State#csi_service_state.stats_requests_exclude),
    % second include it in stats requests if it does not contain all
    NewState =
        case lists:member(all,
                          State#csi_service_state.stats_requests_include) of
            true ->
                State#csi_service_state{stats_requests_exclude = ExcludeList};
            _ ->
                % add to the list of requests if it is not there
                IncludeList =
                    csi_utils:add_elems_to_list(
                      FunctionList,
                      State#csi_service_state.stats_requests_include),
                State#csi_service_state{stats_requests_exclude = ExcludeList,
                                        stats_requests_include = IncludeList}
        end,
    {reply, ok, NewState};

handle_call({'$stats_exclude_funs', FunctionList}, From, State)
  when is_atom(FunctionList) ->
    handle_call({'$stats_exclude_funs', [FunctionList]}, From, State);

handle_call({'$stats_exclude_funs', FunctionList}, _From, State) ->
    % first, remove it from the requests
    IncludeList =
        csi_utils:remove_elems_from_list(
          FunctionList,
          State#csi_service_state.stats_requests_include),
    % second, add it to the exclude list
    ExcludeList = csi_utils:add_elems_to_list(
                    FunctionList,
                    State#csi_service_state.stats_requests_exclude),
    NewState = State#csi_service_state{stats_requests_exclude = ExcludeList,
                                       stats_requests_include = IncludeList},
    {reply, ok, NewState};

handle_call({'$stats_set_funs', FunctionList}, From, State)
  when is_atom(FunctionList) ->
    handle_call({'$stats_set_funs', [FunctionList]}, From, State);

handle_call({'$stats_set_funs', FunctionList}, _From, State) ->
    NewState = State#csi_service_state{stats_requests_include = FunctionList,
                                       stats_requests_exclude = []},
    {reply, ok, NewState};

handle_call('$stats_get_all', _From, State) ->
%%     Reply = lists:flatten(ets:foldl(fun (X, AccIn) ->
%%                                         [io_lib:format("~p~n", [X])|AccIn]
%%                                     end, [], State#rstate.stat_table)),
    Reply = ets:tab2list(State#csi_service_state.stats_table),
    {reply, Reply, State};

handle_call({'$stats_get_funs', FunctionList}, _From, State)
    when is_atom(FunctionList) ->
        handle_call({'$stats_get_funs', [FunctionList]}, _From, State);

handle_call({'$stats_get_funs', _FunctionList}, _From, State) ->
    %% @TODO return the stats for only the functions in the Functionlist
    Reply = ets:tab2list(State#csi_service_state.stats_table),
    {reply, Reply, State};

handle_call({'$stats_get_types', TypeList}, _From, State)
    when is_atom(TypeList) ->
        handle_call({'$stats_get_types', [TypeList]}, _From, State);

handle_call({'$stats_get_types', _TypeList}, _From, State) ->
    %% @TODO return the stats for only the types in the Typelist
    Reply = ets:tab2list(State#csi_service_state.stats_table),
    {reply, Reply, State};

handle_call({'$stats_get_specific', _Function, _Type}, _From, State) ->
    %% @TODO return the stats for only the function for a type
    Reply = ets:tab2list(State#csi_service_state.stats_table),
    {reply, Reply, State};

handle_call('$stats_get_process_table', _From, State) ->
    Reply = ets:tab2list(State#csi_service_state.stats_process_table),
    {reply, Reply, State};

handle_call({'$stats_include_type', TypeList}, From, State)
  when is_atom(TypeList) ->
    handle_call({'$stats_include_type', [TypeList]}, From, State);

handle_call({'$stats_include_type', TypeList}, _From, State) ->
    IncludeList = csi_utils:add_elems_to_list(
                    TypeList,
                    State#csi_service_state.stats_types),
    NewState = State#csi_service_state{stats_types = IncludeList},
    {reply, ok, NewState};

handle_call({'$stats_exclude_type', TypeList}, From, State)
  when is_atom(TypeList) ->
    handle_call({'$stats_exclude_type', [TypeList]}, From, State);

handle_call({'$stats_exclude_type', TypeList}, _From, State) ->
    % first, remove it from the requests
    ExcludeList = csi_utils:remove_elems_from_list(
                    TypeList,
                    State#csi_service_state.stats_types),
    NewState = State#csi_service_state{stats_types = ExcludeList},
    {reply, ok, NewState};

handle_call({'$stats_change_module', Module}, _From, State) ->
    %% @TODO check if the module is loaded and load it if not

    NewState = State#csi_service_state{stats_module = Module},
    {reply, ok, NewState};

handle_call({'$stats_params'}, _From, State) ->
    {reply, State#csi_service_state.stats_types, State};

handle_call({'$stats_params', Type}, _From, State) ->
    {reply, proplists:get_value(Type,
                                State#csi_service_state.stats_types),
     State};

handle_call({'$stats_param_get', Type, Parameter}, _From, State) ->
    {reply,
     proplists:get_value(
       Parameter,
       proplists:get_value(Type, State#csi_service_state.stats_types)),
     State};

handle_call({'$stats_param_set', Type, Parameter, Value}, _From, State) ->
    NewValues =
        lists:keyreplace(Parameter,
                         1,
                         proplists:get_value(
                           Type,
                           State#csi_service_state.stats_types),
                         UpdatedValue =
                             {Parameter, Value}),
    NewStats = lists:keyreplace(Type,
                                1,
                                State#csi_service_state.stats_types,
                                {Type, NewValues}),
    {reply, UpdatedValue, State#csi_service_state{stats_types = NewStats}};

handle_call({call_p, Request, Args, TimeoutForProcessing} = R, From, State) ->
    collect_stats(start, State, Request, R, Ref = make_ref()),
    Pid = proc_lib:spawn_link(?MODULE,
                              process_service_request,
                              [From, State#csi_service_state.service_module,
                               Request, Args, State,
                               find_timeout(TimeoutForProcessing, State),
                               self(), true]),
    ets:insert(State#csi_service_state.stats_process_table, {Pid,
                                                             Ref,
                                                             Request,
                                                             R}),
    {noreply, State};

handle_call({call_s, Request, Args} = R, _From, State) ->
    collect_stats(start, State, Request, R, Ref = make_ref()),
    Module = State#csi_service_state.service_module,
    try case Module:init(Args, State#csi_service_state.service_state) of
            {ok, PState} ->
                {Reply, EState} = Module:Request(Args, PState),
                Module:terminate(normal, EState),
                collect_stats(stop, State, Request, R, Ref),
                {reply, Reply, State};
            WAFIT ->
               collect_stats(clean, State, Request, R, Ref),
               {reply, WAFIT, State}
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
            collect_stats(clean, State, Request, R, Ref),
            {reply, {error, exception}, State}
    end;

handle_call({post_p, Request, Args, TimeoutForProcessing} = R, From, State) ->
    collect_stats(start, State, Request, R, Ref = make_ref()),
    Pid = proc_lib:spawn_link(?MODULE,
                              process_service_request,
                              [From, State#csi_service_state.service_module,
                               Request, Args, State,
                               find_timeout(TimeoutForProcessing, State),
                               self(), true]),
    ets:insert(State#csi_service_state.stats_process_table,
               {Pid, Ref, Request, R}),
    {reply, {posted, Pid, From}, State};

handle_call({cast_p, Request, Args, TimeoutForProcessing} = R, From, State) ->
    collect_stats(start, State, Request, R, Ref = make_ref()),
    Pid = proc_lib:spawn_link(?MODULE,
                              process_service_request,
                              [From, State#csi_service_state.service_module,
                               Request, Args, State,
                               find_timeout(TimeoutForProcessing, State),
                               self(), false]),
    ets:insert(State#csi_service_state.stats_process_table,
               {Pid, Ref, Request, R}),
    {reply, {casted, Pid} , State};

handle_call({Request, Args}, From, State) ->
    collect_stats(start, State, Request, Request, Ref = make_ref()),
    Module = State#csi_service_state.service_module,
    try
        ReturnValue = Module:handle_call({Request, Args},
                                         From,
                                         State#csi_service_state.service_state),
        collect_stats(stop, State, Request, Request, Ref),
        case  ReturnValue of
            {reply, Reply, NewState} ->
                {reply, Reply,
                 State#csi_service_state{service_state = NewState}};
            {reply, Reply, NewState, Timeout} ->
                {reply, Reply,
                 State#csi_service_state{service_state = NewState}, Timeout};
            {noreply, NewState} ->
                {noreply, State#csi_service_state{service_state = NewState}};
            {noreply, NewState, Timeout} ->
                {noreply,
                 State#csi_service_state{service_state = NewState}, Timeout};
            {stop, Reason, Reply, NewState} ->
                {stop, Reason, Reply,
                 State#csi_service_state{service_state = NewState}};
            {stop, Reason, NewState} ->
                {stop, Reason,
                 State#csi_service_state{service_state = NewState}}
        end
    catch
        A:B ->
            ?LOGFORMAT(error,
                       "Exception in service when calling ~p:handle_call(~p)."
                       " ~p:~p. Stacktrace:~p~n",
                       [Module,
                        Request,
                        A,
                        B,
                        erlang:get_stacktrace()]),
            collect_stats(clean, State, Request, Request, Ref),
            {reply, {error, exception}, State}
    end.

process_service_request(From, Module, Request, Args, State,
                        TimeoutForProcessing, Parent, NeedReply) ->
    TRef = case TimeoutForProcessing of
               infinity ->
                   undefined;
               RealTimeout ->
                   KillMessage = case NeedReply of
                                     true ->
                                         {kill_worker_reply,
                                          self(),
                                          From,
                                          Module,
                                          Request,
                                          Args};
                                     false ->
                                         {kill_worker_noreply,
                                          self(),
                                          Module,
                                          Request,
                                          Args}
                                 end,
                   erlang:send_after(RealTimeout,
                                     Parent,
                                     KillMessage)
           end,
    try case Module:init(Args, State#csi_service_state.service_state) of
            {ok, PState} ->
                {Reply, EState} = Module:Request(Args, PState),
                case NeedReply of
                    true ->
                        gen_server:reply(From, Reply);
                    _ ->
                        ok
                end,
                Module:terminate(normal, EState);
            WAFIT ->
                gen_server:reply(From, WAFIT)
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
            catch gen_server:reply(From, {error, exception}),
            _ = case TRef of
                    undefined ->
                        ok;
                    RealTRef0 ->
                        erlang:cancel_timer(RealTRef0)
                end,
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
handle_cast({cast, FunctionRequest, Args}, State) ->
    _ = handle_call({FunctionRequest, Args}, {self(), make_ref()}, State),
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
    case try (State#csi_service_state.service_module):
             handle_info(Info, State#csi_service_state.service_state)
         catch
             _:_ ->
                 continue
         end of
        continue ->
            case Info of
                {'EXIT', Pid, Reason} ->
                    case ets:lookup(
                           State#csi_service_state.stats_process_table, Pid) of
                        [{Pid, Ref, Request, R}] ->
                            case Reason =:= normal of
                                true ->
                                    collect_stats(stop, State, Request, R, Ref);
                                _ ->
                                    collect_stats(clean,
                                                  State,
                                                  undefined,
                                                  undefined,
                                                  Ref)
                            end,
                            ets:delete(
                              State#csi_service_state.stats_process_table, Pid);
                        WAFIT ->
                            ?LOGFORMAT(warning,
                                       "Pid ~p not returned value ~p"
                                       " from process table~n",
                                       [Pid, WAFIT])
                    end;
                {kill_worker_reply, Pid, CallerPid, Module, Request, Args} ->
                    catch gen_server:reply(CallerPid, {error, timeout_killed}),
                    ?LOGFORMAT(warning,
                               "Worker killed with reply. Pid:~p. "
                               "Called function was: ~p:~p(~p)~n",
                               [Pid, Module, Request, Args]),
                    erlang:exit(Pid, kill);
                {kill_worker_noreply, Pid, Module, Request, Args} ->
                    ?LOGFORMAT(warning, "Worker killed with no reply Pid:~p. "
                               "Called function was: ~p:~p(~p)~n",
                               [Pid, Module, Request, Args]),
                    erlang:exit(Pid, kill);
                WAFIT ->
                    ?LOGFORMAT(warning,
                               "Unhandled message received for service ~p."
                               "Module to be called:~p. Message:~p",
                               [State#csi_service_state.service_name,
                                State#csi_service_state.service_module,
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
terminate(Reason, #csi_service_state{service_module = Module,
                                     service_name = Name,
                                     service_state = ServiceState,
                                     stats_table = StatTable,
                                     stats_temp_table = TempStatTable,
                                     stats_process_table = ProcessTable}) ->
    ets:delete(StatTable),
    ets:delete(ProcessTable),
    ets:delete(TempStatTable),
    try Module:terminate_service(Reason, ServiceState)
    catch
        A:B ->
            ?LOGFORMAT(error,
                       "Exception when calling terminate_service for"
                       " service ~p as"
                       "~p:terminate_service(~p, ~p). ~p:~p. Stacktrace:~p",
                       [Name,
                        Module,
                        Reason,
                        ServiceState,
                        A,
                        B,
                        erlang:get_stacktrace()]),
            {error, exception}
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

collect_stats(Stage, State, Request, R, Ref) ->
    case State#csi_service_state.stats_collect of
        true ->
            TimeStamp = csi_utils:now_usec(),
            case stats_to_collect(Request, State) of
                true ->
                    try
                        lists:foreach(
                          fun ({M, Params}) ->
                                   (State#csi_service_state.stats_module):
                                   M(Stage,
                                     Request,
                                     R,
                                     Ref,
                                     Params,
                                     State#csi_service_state.stats_table,
                                     State#csi_service_state.stats_temp_table,
                                     TimeStamp
                                    )
                          end,
                          State#csi_service_state.stats_types)
                    catch
                        A:B ->
                            ?LOGFORMAT(error,
                                       "Exception when calling "
                                       "~p:~p(~p). ~p:~p --> Stacktrace:~p",
                                       [State#csi_service_state.stats_module,
                                        State#csi_service_state.stats_types,
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

stats_to_collect(Request, State) ->
    case lists:member(all, State#csi_service_state.stats_requests_include) of
        true ->
            not lists:member(Request,
                             State#csi_service_state.stats_requests_exclude);
        _ ->
            lists:member(Request,
                         State#csi_service_state.stats_requests_include) and
                not lists:member(Request,
                                 State#csi_service_state.stats_requests_exclude)
    end.

find_timeout(TimeOut, State) ->
    case TimeOut of
        undefined ->
            State#csi_service_state.service_timeout;
        default ->
            ?DEFAULT_SERVICE_TIMEOUT;
        _ ->
            TimeOut
    end.
