%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% supervisor for CSI service
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------
-module(csi_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CSIServer = {csi,{csi,start_link,[]},
                  permanent,2000,worker,[csi]},
	Procs = [CSIServer],
    {ok,{{one_for_one,3,10}, Procs}}.