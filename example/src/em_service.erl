-module(em_service).
-behaviour(csi_server).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
%% General state of the service
-record(em_state,{}).

%% Lifecycle State for every requests'
-record(em_session_state{}).

-export([init_service/1,
         init/2,
         terminate/2,
         terminate_service/2]).

init_service(_InitArgs) ->
    {ok,#em_state{}}.

init(_Args,ServiceState) ->
    {ok,#em_session_state{}}.

terminate(Reason,_State) ->
    ok.
    
terminate_service(_Reason,_State) ->
    ok.

%% ====================================================================
%% Service functions
%% ====================================================================
-export([process_foo/2,
         process_too_long/2,
         process_crashing/2]).

process_foo(_Args,State) ->
    {hello_world,State}.

process_too_long(_Args,State) ->
    timer:sleep(100000),
    {long_job_fininshed,State}.

process_crashing(Args,State) ->
    A = Args - Args,
    {A,State}.
