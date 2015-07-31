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

-export([process_foo/1,
         process_too_long/1,
         process_crashing/1]).


start() -> csi_server:start(?CSI_SERVICE_NAME,?CSI_SERVICE_MODULE).
start_link() -> cu_pserver:start_link(?CSI_SERVICE_NAME,?CSI_SERVICE_MODULE).

stop() -> cu_pserver:stop(?CSI_SERVICE_NAME).

process_foo(From) -> csi_server:call(?CSI_SERVICE_NAME,process_foo,From).
process_too_long(From) -> csi_pserver:call(?CSI_SERVICE_NAME,process_too_long,From,4000).
process_crashing(From) -> csi_pserver:call(?CSI_SERVICE_NAME,process_crashing,From).
