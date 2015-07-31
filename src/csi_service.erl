%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% erls app.erl doc header
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

-export([process_foo/2,
         process_too_long/2,
         process_crashing/2]).

% init the global service
init_service(_InitArgs) ->
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

