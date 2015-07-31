%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% Common service interface application
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------

-module(csi).
-include("csi.hrl").
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
          call_p/3,
          call_p/4,
          call_s/3,
          call_s/4,
          call/2,
          call/3,
          cast_p/3,
          cast_p/4,
%%           cast_f/3,
%%           cast_f/4,
%%           cast/2,
%%           cast/3,
          post_p/3,
          post_p/4,
          stats_start/1,
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
          stats_change_module/2
        ])
.

-export([process_foo/1,
         process_too_long/1,
         process_crashing/1]).


start() -> start(?CSI_SERVICE_NAME,?CSI_SERVICE_MODULE).
start_link() -> start_link(?CSI_SERVICE_NAME,?CSI_SERVICE_MODULE).

stop() -> stop(?CSI_SERVICE_NAME).

start(ServerName, Module) -> start(ServerName, Module, undefined).
start(ServerName, Module, InitArgs) ->
    gen_server:start({local, ServerName},
                     ?CSI_SERVER_MODULE, 
                     {ServerName, Module, InitArgs},
                     [])
.
start_link(ServerName, Module) -> start_link(ServerName, Module, []).
start_link(ServerName, Module, InitArgs) ->
    gen_server:start_link({local, ServerName},
                          ?CSI_SERVER_MODULE,
                          {ServerName, Module, InitArgs},
                          []).

stop(ServerName) ->
    gen_server:call(ServerName,
                    stop,
                    ?DEFAULT_SERVER_TIMEOUT).

stats_start(ServerName) ->
    gen_server:call(ServerName,
                    '$stats_start',
                    ?DEFAULT_SERVER_TIMEOUT).

stats_stop(ServerName) ->
    gen_server:call(ServerName,
                    '$stats_stop',
                    ?DEFAULT_SERVER_TIMEOUT).

stats_include_funs(ServerName,FunctionList) ->
    gen_server:call(ServerName,{'$stats_include_funs',
                                FunctionList},
                    ?DEFAULT_SERVER_TIMEOUT).

stats_exclude_funs(ServerName,FunctionList) ->
    gen_server:call(ServerName,
                    {'$stats_exclude_funs',FunctionList},
                    ?DEFAULT_SERVER_TIMEOUT).
    
stats_get_all(ServerName) ->
    gen_server:call(ServerName,
                    '$stats_get_all',
                    ?DEFAULT_SERVER_TIMEOUT).

stats_get_funs(ServerName,FunctionList) ->
    gen_server:call(ServerName,
                    {'$stats_get_funs',FunctionList},
                    ?DEFAULT_SERVER_TIMEOUT).

stats_get_types(ServerName,TypeList) ->
    gen_server:call(ServerName,
                    {'$stats_get_type',TypeList},
                    ?DEFAULT_SERVER_TIMEOUT).

stats_get_specific(ServerName,Function,Type) ->
    gen_server:call(ServerName,
                    {'$stats_get_specific',Function,Type},
                    ?DEFAULT_SERVER_TIMEOUT).

stats_get_process_table(ServerName) ->
    gen_server:call(ServerName,
                    '$stats_get_process_table',
                    ?DEFAULT_SERVER_TIMEOUT).

stats_set_funs(ServerName,FunctionList) ->
    gen_server:call(ServerName,
                    {'$stats_set_funs',FunctionList},
                    ?DEFAULT_SERVER_TIMEOUT).

stats_include_type(ServerName,Type) ->
    gen_server:call(ServerName,
                    {'$stats_include_type',Type},
                    ?DEFAULT_SERVER_TIMEOUT).

stats_exclude_type(ServerName,Type) ->
    gen_server:call(ServerName,
                    {'$stats_exclude_type',Type},
                    ?DEFAULT_SERVER_TIMEOUT).

stats_change_module(ServerName,Module) ->
    gen_server:call(ServerName,
                    {'$stats_change_module',Module},
                    ?DEFAULT_SERVER_TIMEOUT).

call_p(ServerName, Request, Args) ->
    call_p(ServerName, Request,Args,?DEFAULT_SERVER_TIMEOUT)
.
call_p(ServerName, Request, Args, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                          {call_p, Request, Args, TimeoutForProcessing},
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          ?CALCULATED_SERVER_TIMEOUT(TimeoutForProcessing))
      .

cast_p(ServerName, Request, Args) ->
    cast_p(ServerName, Request, Args, ?DEFAULT_SERVER_TIMEOUT).
cast_p(ServerName, Request, Args, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                          {cast_p, Request, Args, TimeoutForProcessing},
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          ?CALCULATED_SERVER_TIMEOUT(TimeoutForProcessing))
.

post_p(ServerName, Request, Args) ->
    post_p(ServerName, Request, Args, ?DEFAULT_SERVER_TIMEOUT).
post_p(ServerName, Request, Args, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                          {post_p, Request, Args, TimeoutForProcessing},
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          ?CALCULATED_SERVER_TIMEOUT(TimeoutForProcessing))
      .

call_s(ServerName, Request, Args) ->
    call_s(ServerName, Request,Args,?DEFAULT_SERVER_TIMEOUT)
.
call_s(ServerName, Request, Args, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                          {call_s, Request, Args},
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          ?CALCULATED_SERVER_TIMEOUT(TimeoutForProcessing))
      .

call(ServerName, Request) ->
    call(ServerName, Request, ?DEFAULT_SERVER_TIMEOUT)
.
call(ServerName, Request, TimeoutForProcessing) ->
    csi_utils:call_server(ServerName,
                          Request,
                          ?DEFAULT_SERVICE_RETRY,
                          ?DEFAULT_SERVICE_SLEEP,
                          ?CALCULATED_SERVER_TIMEOUT(TimeoutForProcessing))
.

%% @TODO implement cast_server in cu
%% cast_f(ServerName, Request, Args) ->
%%     cast_f(ServerName, Request, Args, ?DEFAULT_SERVER_TIMEOUT).
%% cast_f(ServerName, Request, Args, TimeoutForProcessing) ->
%%     csi_utils:call_server(ServerName,
%%                    {cast_f, Request, Args, TimeoutForProcessing},
%%                    ?DEFAULT_SERVICE_RETRY,
%%                    ?DEFAULT_SERVICE_SLEEP,
%%                    ?CALCULATED_SERVER_TIMEOUT(TimeoutForProcessing))
%% .
%% 
%% cast(ServerName, Request) ->
%%     cast(ServerName, Request, ?DEFAULT_SERVER_TIMEOUT).
%% cast(ServerName, Request, TimeoutForProcessing) ->
%%     csi_utils:cast_server(ServerName,
%%                    Request,
%%                    ?DEFAULT_SERVICE_RETRY,
%%                    ?DEFAULT_SERVICE_SLEEP,
%%                    ?CALCULATED_SERVER_TIMEOUT(TimeoutForProcessing))
%% .


process_foo(From) -> csi:call_p(?CSI_SERVICE_NAME,process_foo,From).
process_too_long(From) -> csi:call_p(?CSI_SERVICE_NAME,process_too_long,From,4000).
process_crashing(From) -> csi:call_p(?CSI_SERVICE_NAME,process_crashing,From).
