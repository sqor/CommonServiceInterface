%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% Common service interface application
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------

-module(csi).
-compile([{parse_transform, lager_transform}, {export_all}]).

-include("csi.hrl").
-include("csi_common.hrl").
%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,
         start_link/0,
         stop/0]).

-export([ start/2,
          start_link/2,
          start/3,
          start_link/3,
          stop/1,
          services/0,
          services_status/0,
          service_status/1,
          call_p/2,
          call_p/3,
          call_p/4,
          call_s/2,
          call_s/3,
          call_s/4,
          call/3,
          call/4,
          cast_p/3,
          cast_p/4,
          call_p/5,
          cast/2,
          post_p/3,
          post_p/4,
          stats_start/0,
          stats_start/1,
          stats_stop/0,
          stats_stop/1,
          stats_include_funs/2,
          stats_exclude_funs/2,
          stats_set_funs/2,
          stats_get_all/1,
          stats_get_funs/2,
          stats_get_types/2,
          stats_get_specific/3,
          stats_include_type/2,
          stats_exclude_type/2,
          stats_get_process_table/1,
          stats_change_module/2,
          stats_params/1,
          stats_params/2,
          stats_param_get/3,
          stats_param_set/4,
          register/0,
          unregister/0
        ]).

-export([list_macros/0,
         process_foo_call_p/1,
         process_too_long_call_p/1,
         process_crashing_call_p/1,
         process_foo_call_s/1,
         process_too_long_call_s/1,
         process_crashing_call_s/1,
         process_foo_call/1,
         process_too_long_call/1,
         process_crashing_call/1,
         process_foo_post_p/1,
         process_too_long_post_p/1,
         process_crashing_post_p/1,
         process_foo_cast/1,
         process_too_long_cast/1,
         process_crashing_cast/1
        ]).


%% start/0
%% ====================================================================
%% @doc starts the Common Service Interface service
%% @end
-spec start() -> Result when
    Result :: {ok, Pid} | ignore | {error, Error},
    Pid :: pid(),
    Error :: {already_started, Pid} | term().
%% ====================================================================
start() -> start(?CSI_SERVICE_NAME, ?CSI_SERVICE_MODULE).

%% start_link/0
%% ====================================================================
%% @doc starts the Common Service Interface service and make a link
%% @end
-spec start_link() -> Result when
    Result :: {ok, Pid} | ignore | {error, Error},
    Pid :: pid(),
    Error :: {already_started, Pid} | term().
%% ====================================================================
start_link() -> start_link(?CSI_SERVICE_NAME, ?CSI_SERVICE_MODULE).

%% stop/0
%% ====================================================================
%% @doc stops the Common Service Interface service
%% @end
-spec stop() -> Reply when
    Reply :: term().
%% ====================================================================
stop() -> stop(?CSI_SERVICE_NAME).

%% start/2
%% ====================================================================
%% @doc starts a service through the Common Service Interface service
%% @end
-spec start(ServerName :: atom(),
            Module :: atom()) -> Result when
    Result :: {ok, Pid} | ignore | {error, Error},
    Pid :: pid(),
    Error :: {already_started, Pid} | term().
%% ====================================================================
start(ServerName, Module) ->
    start(ServerName, Module, undefined).

%% start/3
%% ====================================================================
%% @doc starts a service through the Common Service Interface service
%% @end
-spec start(ServerName :: atom(),
            Module :: atom(),
            InitArgs :: term()) -> Result when
    Result :: {ok, Pid} | ignore | {error, Error},
    Pid :: pid(),
    Error :: {already_started, Pid} | term().
%% ====================================================================
start(ServerName, Module, InitArgs) ->
    gen_server:start({local, ServerName},
                     ?CSI_SERVER_MODULE,
                     {ServerName, Module, InitArgs},
                     []).

%% start_link/2
%% ====================================================================
%% @doc starts a service through the Common Service Interface service with link
%% @end
-spec start_link(ServerName :: atom(),
            Module :: atom()) -> Result when
    Result :: {ok, Pid} | ignore | {error, Error},
    Pid :: pid(),
    Error :: {already_started, Pid} | term().
%% ====================================================================

start_link(ServerName, Module) ->
    start_link(ServerName, Module, []).

%% start_link/3
%% ====================================================================
%% @doc starts a service through the Common Service Interface service
%% @end
-spec start_link(ServerName :: atom(),
            Module :: atom(),
            InitArgs :: term()) -> Result when
    Result :: {ok, Pid} | ignore | {error, Error},
    Pid :: pid(),
    Error :: {already_started, Pid} | term().
%% ====================================================================
start_link(ServerName, Module, InitArgs) ->
    gen_server:start_link({local, ServerName},
                          ?CSI_SERVER_MODULE,
                          {ServerName, Module, InitArgs},
                          []).

%% stop/1
%% ====================================================================
%% @doc stops a service through the Common Service Interface service
%% @end
-spec stop(ServerName :: atom()) -> Reply when
    Reply :: term().
%% ====================================================================
stop(ServerName) ->
    gen_server:call(ServerName,
                    stop,
                    ?DEFAULT_SERVER_TIMEOUT).

%% services/0
%% ====================================================================
%% @doc lists registered services for CSI
%% @end
-spec services() -> Reply when
    Reply :: term(). %list({registered_name, atom()}).
%% ====================================================================
services() ->
    csi:call_s(?CSI_SERVICE_NAME,
               services).

%% services_status/0
%% ====================================================================
%% @doc collect all CSI services status
%% @end
-spec services_status() -> Reply when
    Reply :: list({{registered_name, atom()}, csi_server:csi_service_state()}).
%% ====================================================================
%% [{{registered_name, csi_service},
%%   {csi_service_state, csi_service, csi_service,
%%                      {csi_service_state},
%%                      true, 20500, 24597, csi_server,
%%                      [all],
%%                      [],
%%                      [{response_time, [{"last_nth_to_collect", 10},
%%                                        {"normalize_to_nth", 8}]}]}}]
services_status() ->
    gen_server:call(?CSI_SERVICE_NAME,
                    '$collect_services_status',
                    ?DEFAULT_SERVER_TIMEOUT).

%% service_status/1
%% ====================================================================
%% @doc returns a CSI service status
%% @end
-spec service_status(ServiceName :: atom()) -> Reply when
    Reply :: csi_server:csi_service_state().
%% ====================================================================
service_status(ServerName) ->
    gen_server:call(ServerName,
                    '$service_status',
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_start/0
%% ====================================================================
%% @doc turns collecting statistics for all CSI services
%% @end
-spec stats_start() -> Reply when
    Reply :: term().
%% ====================================================================
stats_start() ->
    gen_server:call(?CSI_SERVICE_NAME,
                    '$stats_start_all',
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_start/1
%% ====================================================================
%% @doc turns collecting statistics for a named CSI service
%% @end
-spec stats_start(ServerName :: atom()) -> Reply when
    Reply :: term().
%% ====================================================================
stats_start(ServerName) ->
    gen_server:call(ServerName,
                    '$stats_start',
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_stop/0
%% ====================================================================
%% @doc turns off collecting statistics for all CSI services
%% @end
-spec stats_stop() -> Reply when
    Reply :: term().
%% ====================================================================
stats_stop() ->
    gen_server:call(?CSI_SERVICE_NAME,
                    '$stats_stop_all',
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_stop/1
%% ====================================================================
%% @doc turns off collecting statistics for a named CSI service
%% @end
-spec stats_stop(ServerName :: atom()) -> Reply when
    Reply :: term().
%% ====================================================================
stats_stop(ServerName) ->
    gen_server:call(ServerName,
                    '$stats_stop',
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_include_funs/2
%% ====================================================================
%% @doc include functions to collect stats for
%% @end
-spec stats_include_funs(ServerName :: atom(),
                         FunctionList :: list(atom())) -> Reply when
    Reply :: term().
%% ====================================================================
stats_include_funs(ServerName, FunctionList) ->
    gen_server:call(ServerName, {'$stats_include_funs',
                                 FunctionList},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_exclude_funs/2
%% ====================================================================
%% @doc exclude functions from included functions to collect stats for
%% @end
-spec stats_exclude_funs(ServerName :: atom(),
                         FunctionList :: list(atom())) -> Reply when
    Reply :: term().
%% ====================================================================
stats_exclude_funs(ServerName, FunctionList) ->
    gen_server:call(ServerName,
                    {'$stats_exclude_funs', FunctionList},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_get_all/1
%% ====================================================================
%% @doc get all statistics for a CSI service
%% @end
-spec stats_get_all(ServerName :: atom()) -> Reply when
    Reply :: list({{Type :: atom(), Function :: atom()}, tuple()}).
%% ====================================================================
%% [{{response_time, process_foo}, {1, 69, 69.0, 69, 69}}]
stats_get_all(ServerName) ->
    gen_server:call(ServerName,
                    '$stats_get_all',
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_get_funs/2
%% ====================================================================
%% @doc get statistics for functions in a CSI service
%% @end
-spec stats_get_funs(ServerName :: atom(),
                     FunctionList :: list(atom()) | atom()) -> Reply when
    Reply :: list({{Type :: atom(), Function :: atom()}, tuple()}).
%% ====================================================================
stats_get_funs(ServerName, FunctionList)
  when is_atom(FunctionList) ->
    stats_get_funs(ServerName, [FunctionList]);

stats_get_funs(ServerName, FunctionList)
  when is_list(FunctionList) ->
    gen_server:call(ServerName,
                    {'$stats_get_funs', FunctionList},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_get_types/2
%% ====================================================================
%% @doc get statistics for a given type in a CSI service
%% @end
-spec stats_get_types(ServerName :: atom(),
                     FunctionList :: list(atom()) | atom()) -> Reply when
    Reply :: list({{Type :: atom(), Function :: atom()}, tuple()}).
%% ====================================================================
stats_get_types(ServerName, TypeList)
  when is_atom(TypeList) ->
    stats_get_types(ServerName, [TypeList]);

stats_get_types(ServerName, TypeList)
  when is_list(TypeList) ->
    gen_server:call(ServerName,
                    {'$stats_get_type', TypeList},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_get_specific/3
%% ====================================================================
%% @doc get statistics for a given type and a given function in a CSI service
%% @end
-spec stats_get_specific(ServerName :: atom(),
                         Function :: atom(),
                         Type :: atom()) -> Reply when
    Reply :: list({{Type :: atom(), Function :: atom()}, tuple()}).
%% ====================================================================
stats_get_specific(ServerName, Function, Type) ->
    gen_server:call(ServerName,
                    {'$stats_get_specific', Function, Type},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_get_process_table/1
%% ====================================================================
%% @doc get the process table for a CSI service
%% @end
-spec stats_get_process_table(ServerName :: atom()) -> Reply when
    Reply :: list(tuple()).
%% ====================================================================
stats_get_process_table(ServerName) ->
    gen_server:call(ServerName,
                    '$stats_get_process_table',
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_set_funs/2
%% ====================================================================
%% @doc sets the list of functions to be included for collecting stats
%% @end
-spec stats_set_funs(ServerName :: atom(),
                     FunctionList :: list(atom())) -> Reply when
    Reply :: term().
%% ====================================================================
stats_set_funs(ServerName, FunctionList)
  when is_list(FunctionList) ->
    gen_server:call(ServerName,
                    {'$stats_set_funs', FunctionList},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_set_param/4
%% ====================================================================
%% @doc sets a parameter for a given statistic type
%% @end
-spec stats_param_set(ServerName :: atom(),
                      Type :: atom(),
                      Parameter :: term(),
                      Value :: term()) -> Reply when
    Reply :: term().
%% ====================================================================
stats_param_set(ServerName, Type, Parameter, Value) ->
    gen_server:call(ServerName,
                    {'$stats_param_set', Type, Parameter, Value},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_param_get/3
%% ====================================================================
%% @doc get a specific parameter for a given statistic type
%% @end
-spec stats_param_get(ServerName :: atom(),
                      Type :: atom(),
                      Parameter :: term()) -> Reply when
                  Reply :: term().
%% ====================================================================
stats_param_get(ServerName, Type, Parameter) ->
    gen_server:call(ServerName,
                    {'$stats_param_get', Type, Parameter},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_params/1
%% ====================================================================
%% @doc get all parameters for a given statistic type
%% @end
-spec stats_params(ServerName :: atom()) -> Reply when
                  Reply :: list(proplists:property()).
%% ====================================================================
stats_params(ServerName) ->
    gen_server:call(ServerName,
                    {'$stats_params'},
                    ?DEFAULT_SERVER_TIMEOUT).


%% stats_params/2
%% ====================================================================
%% @doc get all parameters for a given statistic type
%% @end
-spec stats_params(ServerName :: atom(),
                   Type :: atom()) -> Reply when
                  Reply :: list(proplists:property()).
%% ====================================================================
stats_params(ServerName, Type) ->
    gen_server:call(ServerName,
                    {'$stats_params', Type},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_include_type/2
%% ====================================================================
%% @doc include a statistic type in the collecting metrics
%% The stats_module shall containg the function named exactly as it is
%% in the collected statistics type list
%% @end
-spec stats_include_type(ServerName :: atom(),
                         Type :: atom()) -> Reply when
    Reply :: term().
%% ====================================================================
stats_include_type(ServerName, Type)
  when is_atom(Type) ->
    gen_server:call(ServerName,
                    {'$stats_include_type', Type},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_exclude_type/2
%% ====================================================================
%% @doc stop collecting a statistic type for a service
%% @end
-spec stats_exclude_type(ServerName :: atom(),
                         Type :: atom()) -> Reply when
    Reply :: term().
%% ====================================================================
stats_exclude_type(ServerName, Type)
  when is_atom(Type) ->
    gen_server:call(ServerName,
                    {'$stats_exclude_type', Type},
                    ?DEFAULT_SERVER_TIMEOUT).

%% stats_change_module/2
%% ====================================================================
%% @doc sets the module name for a service where the statistical functions are
%% @end
-spec stats_change_module(ServerName :: atom(),
                          Module :: atom()) -> Reply when
    Reply :: term().
%% ====================================================================
stats_change_module(ServerName, Module) ->
    gen_server:call(ServerName,
                    {'$stats_change_module', Module},
                    ?DEFAULT_SERVER_TIMEOUT).

%% call_p/2
%% ====================================================================
%% @doc call a service function parallel
%% @end
-spec call_p(ServerName :: atom(),
             Request :: atom()) -> Reply when
    Reply :: term().
%% ====================================================================
call_p(ServerName, Request) ->
    call_p(ServerName, Request, [], ?DEFAULT_SERVICE_TIMEOUT).

%% call_p/3
%% ====================================================================
%% @doc call a service function parallel with arguments
%% @end
-spec call_p(ServerName :: atom(),
             Request :: atom(),
             Args :: term()) -> Reply when
    Reply :: term().
%% ====================================================================
call_p(ServerName, Request, Args) ->
    call_p(ServerName, Request, Args, ?DEFAULT_SERVICE_TIMEOUT).

%% call_p/4
%% ====================================================================
%% @doc call a service function parallel with arguments and timeout
%% @end
-spec call_p(ServerName :: atom(),
             Request :: atom(),
             Args :: term(),
             TimeoutForProcessing :: infinity | integer()) -> Reply when
    Reply :: term().
%% ====================================================================
call_p(ServerName, Request, Args, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                          {call_p, Request, Args, TimeoutForProcessing},
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          ?DEFAULT_SERVER_TIMEOUT).

%% call_p/5
%% ====================================================================
%% @doc call a service function parallel with arguments and timeouts
%% @end
-spec call_p(ServerName :: atom(),
             Request :: atom(),
             Args :: term(),
             TimeoutForProcessing :: infinity | non_neg_integer(),
             ServerTimeout :: infinity | non_neg_integer()) -> Reply when
    Reply :: term().
%% ====================================================================
call_p(ServerName, Request, Args, TimeoutForProcessing, ServerTimeout) ->
    csi_utils:call_server(ServerName,
                          {call_p, Request, Args, TimeoutForProcessing},
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          ServerTimeout).

%% call_s/2
%% ====================================================================
%% @doc call a service function serialized
%% @end
-spec call_s(ServerName :: atom(),
             Request :: atom()) -> Reply when
    Reply :: term().
%% ====================================================================
call_s(ServerName, Request) ->
    call_s(ServerName, Request, [], ?DEFAULT_SERVER_TIMEOUT).

%% call_s/3
%% ====================================================================
%% @doc call a service function serialized with arguments
%% @end
-spec call_s(ServerName :: atom(),
             Request :: atom(),
             Args :: term()) -> Reply when
    Reply :: term().
%% ====================================================================
call_s(ServerName, Request, Args) ->
    call_s(ServerName, Request, Args, ?DEFAULT_SERVER_TIMEOUT).

%% call_s/4
%% ====================================================================
%% @doc call a service function serialized with arguments and timeout
%% @end
-spec call_s(ServerName :: atom(),
             Request :: atom(),
             Args :: term(),
             TimeoutForProcessing :: infinity | integer()) -> Reply when
    Reply :: term().
%% ====================================================================
call_s(ServerName, Request, Args, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                          {call_s, Request, Args},
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          TimeoutForProcessing).

%% call/3
%% ====================================================================
%% @doc call the handle_call service function serialized
%% @end
-spec call(ServerName :: atom(),
           Request :: term(),
           Args :: term()) -> Reply when
    Reply :: term().
%% ====================================================================
call(ServerName, Request, Args) ->
    call(ServerName, Request, Args, ?DEFAULT_SERVER_TIMEOUT).

%% call/4
%% ====================================================================
%% @doc call the handle_call service function serialized with timeout
%% @end
-spec call(ServerName :: atom(),
           Request :: term(),
           Args :: term(),
           TimeoutForProcessing :: infinity | integer()) -> Reply when
    Reply :: term().
%% ====================================================================
call(ServerName, Request, Args, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                          {Request, Args},
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          TimeoutForProcessing).

%% post_p/3
%% ====================================================================
%% @doc post a request parallel
%% @end
-spec post_p(ServerName :: atom(),
             Request :: atom(),
             Args :: term()) -> Reply when
    Reply :: term().
%% ====================================================================
post_p(ServerName, Request, Args) ->
    post_p(ServerName, Request, Args, ?DEFAULT_SERVICE_TIMEOUT).

%% post_p/4
%% ====================================================================
%% @doc post a request parallel with timeout
%% @end
-spec post_p(ServerName :: atom(),
             Request :: atom(),
             Args :: term(),
             TimeoutForProcessing :: infinity | integer()) -> Reply when
    Reply :: term().
%% ====================================================================
post_p(ServerName, Request, Args, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                          {post_p, Request, Args, TimeoutForProcessing},
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          ?DEFAULT_SERVER_TIMEOUT).
%% @TODO implement cast_server in cu
cast_p(ServerName, Request, Args) ->
    cast_p(ServerName, Request, Args, ?DEFAULT_SERVICE_TIMEOUT).
cast_p(ServerName, Request, Args, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                   {cast_p, Request, Args, TimeoutForProcessing},
                   ?DEFAULT_SERVICE_RETRY,
                   ?DEFAULT_SERVICE_SLEEP,
                   ?DEFAULT_SERVER_TIMEOUT).

%% cast/2
%% ====================================================================
%% @doc cast a service request
%% @end
-spec cast(ServerName :: atom(),
           Request :: term()) -> Reply when
    Reply :: term().
%% ====================================================================
cast(ServerName, Request) ->
     gen_server:cast(ServerName,
                     {cast, Request}).

%% register/0
%% ====================================================================
%% @doc registers a service in CSI
%% @end
-spec register() -> Reply when
    Reply :: term().
%% ====================================================================
register() ->
    pg2:join(?CSI_SERVICE_PROCESS_GROUP_NAME, self()).

%% unregister/0
%% ====================================================================
%% @doc unregisters a service in CSI
%% @end
-spec unregister() -> Reply when
    Reply :: term().
%% ====================================================================
unregister() ->
    pg2:leave(?CSI_SERVICE_PROCESS_GROUP_NAME, self()).

% Test functions
list_macros() ->
    ?LOGFORMAT(info, "CSI_SERVICE_NAME:~p~n"
               "CSI_SERVICE_MODULE:~p~n"
               "CSI_SERVER_MODULE:~p~n"
               "CSI_SERVICE_PROCESS_GROUP_NAME:~p~n"
               "DEFAULT_SERVICE_RETRY:~p~n"
               "DEFAULT_SERVICE_SLEEP:~p~n"
               "DEFAULT_SERVER_TIMEOUT:~p~n"
               "LOGTYPE:~p~n",
               [?CSI_SERVICE_NAME,
                ?CSI_SERVICE_MODULE,
                ?CSI_SERVER_MODULE,
                ?CSI_SERVICE_PROCESS_GROUP_NAME,
                ?DEFAULT_SERVICE_RETRY,
                ?DEFAULT_SERVICE_SLEEP,
                ?DEFAULT_SERVER_TIMEOUT,
                ?LOGTYPE
               ]
    ).

process_foo_call(Args) ->
    csi:call(?CSI_SERVICE_NAME,
             process_foo,
             Args).
process_too_long_call(Args) ->
    csi:call(?CSI_SERVICE_NAME,
             process_too_long,
             Args,
             4000).
process_crashing_call(Args) ->
    csi:call(?CSI_SERVICE_NAME,
             process_crashing,
             Args).
process_foo_call_p(Args) ->
    csi:call_p(?CSI_SERVICE_NAME,
               process_foo,
               Args).
process_too_long_call_p(Args) ->
    csi:call_p(?CSI_SERVICE_NAME,
               process_too_long,
               Args,
               4000).
process_crashing_call_p(Args) ->
    csi:call_p(?CSI_SERVICE_NAME,
               process_crashing,
               Args).
process_foo_call_s(Args) ->
    csi:call_s(?CSI_SERVICE_NAME,
               process_foo,
               Args).
process_too_long_call_s(Args) ->
    csi:call_s(?CSI_SERVICE_NAME,
               process_too_long,
               Args,
               4000).
process_crashing_call_s(Args) ->
    csi:call_s(?CSI_SERVICE_NAME,
               process_crashing,
               Args).
process_foo_post_p(Args) ->
    csi:post_p(?CSI_SERVICE_NAME,
               process_foo,
               Args).
process_too_long_post_p(Args) ->
    csi:post_p(?CSI_SERVICE_NAME,
               process_too_long,
               Args,
               4000).
process_crashing_post_p(Args) ->
    csi:post_p(?CSI_SERVICE_NAME,
               process_crashing,
               Args).
process_foo_cast(Args) ->
    csi:cast_p( ?CSI_SERVICE_NAME,
                process_foo, Args).
process_too_long_cast(Args) ->
    csi:cast_p(?CSI_SERVICE_NAME,
               process_too_long,
               Args,
               4000).
process_crashing_cast(Args) ->
    csi:cast_p(?CSI_SERVICE_NAME,
               process_crashing,
               Args).
