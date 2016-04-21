%%%-------------------------------------------------------------------
%%% @author Zsolt Laky <zsolt.laky@erlang-solutions.com>
%%% @copyright (C) 2016, Erlang Solutions.
%%% @doc
%%% Common Service Interface application
%%% @end
%%% Created : 20 Jun 2015 by Erlang Solutions
%%%-------------------------------------------------------------------

-module(csi_sup).
-behaviour(supervisor).

-include("csi.hrl").

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    CSIServer = {?CSI_SERVICE_NAME, {csi, start_link, []},
                  permanent, 2000, worker, [csi]},
    Procs = [CSIServer],
    {ok, {{one_for_one, 3, 10}, Procs}}.
