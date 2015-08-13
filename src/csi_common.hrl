%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% csi common constants
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------
-define(CSI_SERVICE_MODULE,csi_service).
-define(CSI_SERVER_MODULE,csi_server).
-define(CSI_SERVICE_PROCESS_GROUP_NAME,csi_service_group).

-define(DEFAULT_SERVICE_RETRY,2).
-define(DEFAULT_SERVICE_SLEEP,200).

-ifndef(LOGFORMAT).

-ifdef(lager).
-compile([{parse_transform, lager_transform}]).
-define(LOGTYPE,"lager").
-define(LOGFORMAT(Level,Format,Args),
        ok = lager:Level(Format,Args)).
-define(LOGMSG(Level,Format),
        ok = lager:Level(Format)).

-else.
-define(LOGTYPE,"io:format").
-define(LOGFORMAT(Level,Format,Args),
        ok = io:format("~p: ~s",[Level,io_lib:format(Format, Args)])).
-define(LOGMSG(Level,Format),
        ok = io:format("~p: ~p",[Level,Format])).
-endif.
-endif.

-ifdef(debug).
-define(DEFAULT_SERVER_TIMEOUT,infinity).
-define(DEFAULT_SERVICE_TIMEOUT,infinity).
-else.
-define(DEFAULT_SERVER_TIMEOUT,5000).
-define(DEFAULT_SERVICE_TIMEOUT,4000).
-endif.
