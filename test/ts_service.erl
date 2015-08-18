-module(ts_service).
-behaviour(csi_server).

%% General state of the service
-record(ts_state,{}).

%% Lifecycle State for every requests'
-record(ts_session_state,{}).

-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2]).

-export([process_foo/2,
         process_too_long/2,
         process_crashing/2]).


%% ====================================================================
%% Behavioural functions
%% ====================================================================
init_service(_InitArgs) ->
    {ok,#ts_state{}}.

init(_Args,_ServiceState) ->
    {ok,#ts_session_state{}}.

terminate(_Reason,_State) ->
    ok.
    
terminate_service(_Reason,_State) ->
    ok.

%% ====================================================================
%% Service functions
%% ====================================================================
process_foo(_Args,State) ->
    {hello_world,State}.

process_too_long(_Args,State) ->
    timer:sleep(100000),
    {long_job_fininshed,State}.

process_crashing(Args,State) ->
    A = Args - Args,
    {A,State}.
