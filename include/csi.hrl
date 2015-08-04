-compile([{parse_transform, lager_transform},{export_all}]).

-define(CSI_SERVICE_NAME,csi_service).
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
        ok = io:format("~p: ~s",[Level,Format]).
-endif.
-endif.

-ifdef(debug).
-define(DEFAULT_SERVER_TIMEOUT,infinity).
-define(CALCULATED_SERVER_TIMEOUT(Timeout),Timeout+Timeout).
-else.
-define(DEFAULT_SERVER_TIMEOUT,5000).
-define(CALCULATED_SERVER_TIMEOUT(Timeout),Timeout+?DEFAULT_SERVER_TIMEOUT).
-endif.