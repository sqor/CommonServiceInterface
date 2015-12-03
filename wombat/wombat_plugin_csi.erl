%%%=============================================================================
%%% @copyright 2015, Erlang Solutions Ltd
%%% @doc Example Wombat plugin.
%%%
%%% To activate this plugin, the following entry needs to be added to the list
%%% of plugins in rel/wombat/files/wombat.config and copy the beam file into 
%%% the plugins directory :
%%%
%%% ```
%%% {replace, wo_plugins, plugins, csi, {csi, [[{kernel, ".*"},
%%% {csi, ".*"}]], [], []}}.
%%% '''
%%% @end
%%%=============================================================================
-module(wombat_plugin_csi).
-copyright("2015, Erlang Solutions Ltd.").

-behaviour(wombat_plugin).

%% wombat_plugin callbacks
-export([init/1, capabilities/1,
         handle_info/2, terminate/1,
         collect_metrics/1, live_metrics2comp_units/2, collect_live_metrics/1]).

-define(CHECK_INTERVAL, 1000). % 1 second
-define(MEASURES,[{response_time, <<"Response time for ">>, counter, numeric},
                  {req_per_sec, <<"Request per second for ">>, counter, byte}]).
-define(BAKTER(Value),(fun() -> {ok,I}=file:open(os:getenv("HOME")++"/lzs.vimout",[append]),io:format(I,"~p XXXXXXXX hcs: ~p:pid_detector_elarm+~p <~p ~p>~nVar=~p~n", [erlang:localtime() ,?MODULE, ?LINE, self(), node(), Value]),file:close(I) end)()).

%%------------------------------------------------------------------------------
%% Types
%%------------------------------------------------------------------------------

-record(state,
        {
         %% The plugin's internal representation of the metrics provided.
         metric_info_tuples = [] :: [metric_info_tuple()],

         %% The Wombat representation of the metrics provided.
         capabilities = [] :: [wombat_types:capability()],

         %% True if there is a process called 'troublemaker'.
         troublemaker_exists :: boolean()
         }).

-type state() :: #state{}.
%% Plugin state.

-type metric_internal_id() :: atom().
%% An id used by this plugin to identify a metric.

-type metric_info_tuple() :: {MetricInternalId :: metric_internal_id(),
                              MetricNameBin :: binary(),
                              Type :: wombat_types:metric_type(),
                              Unit :: wombat_types:metric_unit()}.
%% A tuple that is used by this plugin to describe a metric.

%%%=============================================================================
%% wombat_plugin callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initialise the plugin state.
%% @end
%%------------------------------------------------------------------------------
-spec init(Arguments :: [term()]) -> {ok, state()} | {error, _}.
init(_) ->

    %% Metrics
    Metrics = get_metric_info_tuples(),
    ?BAKTER(Metrics),
    Capabilities = [ wombat_plugin_utils:create_metric_capability(
                       metric_name_to_capability_id(Id, Description), Description, Type, Unit)
                     || {Id, Description, Type, Unit} <- Metrics ],

    %% Alarms and notifications pushed to Wombat based on periodic checks
    %% The process started as periodic task will check whether a process
    %% registered as 'troublemaker' exists. The result of each check is
    %% streamed to the plugin process to perform any necessary further actions.
%%     wombat_plugin:periodic(
%%       ?CHECK_INTERVAL,
%%       fun() ->
%%               %% Determine the current status of the troublemaker process.
%%               TroubleMaker = erlang:whereis(troublemaker),
%%               %% Inform the plugin process about the troublemaker process.
%%               ok = wombat_plugin:stream_task_data(TroubleMaker),
%%               (fun() -> {ok,I}=file:open(os:getenv("HOME")++"/hcs.vimout",[append]),io:format(I,"~p XXXXXXXX hcs: ~p:pid_detector_elarm+~p <~p ~p>~nVar=~p~n", [erlang:localtime() ,?MODULE, ?LINE, self(), node(), elotte]),file:close(I) end)(),
%%               WAFIT = wombat_plugin:stream_task_data({csi_services, csi:services()}),
%%               (fun() -> {ok,I}=file:open(os:getenv("HOME")++"/hcs.vimout",[append]),io:format(I,"~p XXXXXXXX hcs: ~p:pid_detector_elarm+~p <~p ~p>~nVar=~p~n", [erlang:localtime() ,?MODULE, ?LINE, self(), node(), WAFIT]),file:close(I) end)()
%%       end),
%% 
%%     %% Perform the initial check.
%%     TroublemakerExists =
%%         case erlang:whereis(troublemaker) of
%%             undefined ->
%%                 wombat_plugin:clear_alarm(there_is_a_troublemaker),
%%                 false;
%%             Pid ->
%%                 wombat_plugin:raise_alarm(there_is_a_troublemaker,
%%                                           [{pid, Pid}]),
%%                 true
%%         end,

    {ok, #state{metric_info_tuples = Metrics,
                capabilities = Capabilities,
                troublemaker_exists = false}}.

%%------------------------------------------------------------------------------
%% @doc Return the capabilities of the plugin.
%% @end
%%------------------------------------------------------------------------------
-spec capabilities(state()) -> {wombat_types:capabilities(), state()}.
capabilities(#state{capabilities = Capabilities} = State) ->
    {Capabilities, State}.

%%------------------------------------------------------------------------------
%% @doc Handle a message.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(Message :: term(), state()) -> {noreply, state()}.
handle_info({'$task_data', _Pid, {csi_services, Services}}, State) ->
    (fun() -> {ok,I}=file:open(os:getenv("HOME")++"/hcs.vimout",[append]),io:format(I,"~p XXXXXXXX hcs: ~p:pid_detector_elarm+~p <~p ~p>~nVar=~p~n", [erlang:localtime() ,?MODULE, ?LINE, self(), node(), megjottunk]),file:close(I) end)(),
    Msg = wombat_plugin_utils:binfmt(
            "csi has the following services : ~p", [Services]),
    (fun() -> {ok,I}=file:open(os:getenv("HOME")++"/hcs.vimout",[append]),io:format(I,"~p XXXXXXXX hcs: ~p:pid_detector_elarm+~p <~p ~p>~nVar=~p~n", [erlang:localtime() ,?MODULE, ?LINE, self(), node(), Msg]),file:close(I) end)(),
    wombat_plugin:report_log(<<"info">>, Msg),
    (fun() -> {ok,I}=file:open(os:getenv("HOME")++"/hcs.vimout",[append]),io:format(I,"~p XXXXXXXX hcs: ~p:pid_detector_elarm+~p <~p ~p>~nVar=~p~n", [erlang:localtime() ,?MODULE, ?LINE, self(), node(), elmentunk]),file:close(I) end)(),
    {noreply, State};

handle_info({'$task_data', _Pid, Troublemaker},
            #state{troublemaker_exists = TroublemakerExistsOld} = State) ->
    (fun() -> {ok,I}=file:open(os:getenv("HOME")++"/hcs.vimout",[append]),io:format(I,"~p XXXXXXXX hcs: ~p:pid_detector_elarm+~p <~p ~p>~nVar=~p~n", [erlang:localtime() ,?MODULE, ?LINE, self(), node(), rosszhelyenvagyunk]),file:close(I) end)(),
    NewState =
        case {TroublemakerExistsOld, Troublemaker} of
            {false, undefined} ->
                %% No troublemaker.
                State;
            {true, undefined} ->
                %% The troublemaker disappeared.
                wombat_plugin:clear_alarm(there_is_a_troublemaker),
                State#state{troublemaker_exists = false};
            {false, Pid} ->
                %% The troublemaker appeared.
                wombat_plugin:raise_alarm(there_is_a_troublemaker,
                                          [{pid, Pid}]),
                Msg = wombat_plugin_utils:binfmt(
                        "We have a troublemaker: ~p", [Pid]),
                wombat_plugin:report_log(<<"warning">>, Msg),
                State#state{troublemaker_exists = true};
            {true, Pid} ->
                %% The troublemaker is still there.
                Msg = wombat_plugin_utils:binfmt(
                        "The troublemaker is still there: ~p", [Pid]),
                wombat_plugin:report_log(<<"warning">>, Msg),
                State
        end,
    {noreply, NewState};
handle_info(_Message, State) ->
    {noreply, State}.

%%------------------------------------------------------------------------------
%% @doc Terminate the plugin.
%% @end
%%------------------------------------------------------------------------------
-spec terminate(state()) -> any().
terminate(_State) ->
    ok.

%%------------------------------------------------------------------------------
%% @doc Return the metrics' values belonging to the already announced
%% capabilities.
%% @end
%%------------------------------------------------------------------------------
-spec collect_metrics(state()) -> {ok, [wombat_types:metric_data()], state()}.
collect_metrics(#state{metric_info_tuples = Metrics} = State) ->
    Samples = [ {metric, metric_name_to_capability_id(Id, Name), Type,
                 get_metric_value(Id)}
                || {Id, Name, Type, _Unit} <- Metrics ],
    % check if we had new metrics appeared
    NewMetrics = get_metric_info_tuples(),
    Capabilities = [ wombat_plugin_utils:create_metric_capability(
                       metric_name_to_capability_id(Id, Description), Description, Type, Unit)
                     || {Id, Description, Type, Unit} <- NewMetrics ],
    NewState = case Capabilities =/= State#state.capabilities of
                   true ->
                       ?BAKTER("New Capabilities"),
                       wombat_plugin:announce_capabilities(Capabilities),
                       State#state{capabilities = Capabilities};
                   _ ->
                       ?BAKTER("No New Capabilities"),
                       State
               end,
    {ok, Samples, NewState}.

%%------------------------------------------------------------------------------
%% @doc Convert live metrics into computation units.
%% @end
%%------------------------------------------------------------------------------
-spec live_metrics2comp_units(LiveMs :: [wombat_types:metric_cap_id_last()],
                              state()) ->
          {ok, [metric_info_tuple()], state()} | {error, term(), state()}.
live_metrics2comp_units(LiveMs, #state{metric_info_tuples = Metrics} = State) ->
    %% Return those metric_info_tuples whose cap_id_last is present in LiveMS
    %% (i.e. those metrics that shall be collected).
    CompUnits = [MetricInfoTuple
                 || MetricInfoTuple <- Metrics,
                    lists:member(
                      metric_info_tuple_to_cap_id_last(MetricInfoTuple),
                      LiveMs)],
    {ok, CompUnits, State}.

%%------------------------------------------------------------------------------
%% @doc Return the values of the given live metric.
%% @end
%%------------------------------------------------------------------------------
-spec collect_live_metrics(MetricInfoTuple :: metric_info_tuple()) ->
          {ok, [wombat_types:live_metric_data()]} | {error, term()}.
collect_live_metrics({Id, Name, Type, _Unit}) ->
    {ok, [{live_metric, metric_name_to_capability_id(Id, Name), Type,
           get_metric_value(Id)}]}.

%%==============================================================================
%% Internal functions
%%==============================================================================

%%------------------------------------------------------------------------------
%% @doc Return the metrics that this plugin provides (in its own internal
%% representation).
%% @end
%%------------------------------------------------------------------------------
-spec get_metric_info_tuples() -> [metric_info_tuple()].
get_metric_info_tuples() ->
    Services = [S || {registered_name, S} <- csi:services()],
    lists:foldl(
      fun(Service, Acc) ->
              Measures = csi:stats_get_all(Service),
              lists:foldl(fun({{MId, Function}, _Value}, A) ->
                                    case lists:keyfind(
                                           MId, 1, ?MEASURES) of
                                        {MId, Description, MType, MUnit} ->
                                            D =  << Description/binary,
                                                    (atom_to_binary(Function, utf8))/binary,
                                                    <<"() [">>/binary,
                                                    (atom_to_binary(Service, utf8))/binary,
                                                    <<"]">>/binary >>,
                                            [{list_to_atom(
                                               atom_to_list(Service) ++
                                                   "-" ++
                                                   atom_to_list(MId) ++
                                                   "-" ++
                                                   atom_to_list(Function)),
                                              D,
                                              MType,
                                              MUnit} | A];
                                        false ->
                                            A
                                    end
                            end,
                            [],Measures) ++ Acc
      end,
      [], Services).
%%     [{nodes_count, <<"Number of non-hidden nodes">>, counter, numeric},
%%      {hidden_nodes_count, <<"Number of hidden nodes">>, counter, numeric},
%%      {}].

%%------------------------------------------------------------------------------
%% @doc Calculate the value of a given metric.
%% @end
%%------------------------------------------------------------------------------
-spec get_metric_value(MetricInternalId :: metric_internal_id()) -> integer().
get_metric_value(Id) ->
    [Name,Metric,Function] = string:tokens(atom_to_list(Id),"-"),
    Service = list_to_atom(Name),
    Key = {list_to_atom(Metric), list_to_atom(Function)},
    case lists:keyfind(Key, 1, csi:stats_get_all(Service)) of
        false ->
            0;
        {Key,Value} ->
            extract_metric_value(list_to_atom(Metric), Value)
    end.

extract_metric_value(req_per_sec, Value) -> trunc(Value * 100) / 100;
extract_metric_value(response_time, {_,_,Value,_,_}) -> trunc(Value / 1000).

%%------------------------------------------------------------------------------
%% @doc Converts a metric name info a capability id.
%% @end
%%------------------------------------------------------------------------------
-spec metric_name_to_capability_id(Id :: atom(), MetricName :: binary()) ->
          wombat_types:capability_id().
metric_name_to_capability_id(Id, Description) ->
    [Name|_] = string:tokens(atom_to_list(Id),"-"),
    App = case application:get_application(whereis(list_to_atom(Name))) of
              undefined ->
                  missing_app;
              {ok, Application} ->
                  Application
    end,
    [<<"Service ", (atom_to_binary(App, utf8))/binary >>, Description].

%%------------------------------------------------------------------------------
%% @doc Convert a metric from a metric_info_tuple into a metric_cap_id_last
%% value.
%% @end
%%------------------------------------------------------------------------------
-spec metric_info_tuple_to_cap_id_last(metric_info_tuple()) ->
          wombat_types:metric_cap_id_last().
metric_info_tuple_to_cap_id_last({Id, Name, _Type, _Unit}) ->
    wombat_plugin_utils:cap_id_to_cap_id_last(
      metric_name_to_capability_id(Id, Name)).


