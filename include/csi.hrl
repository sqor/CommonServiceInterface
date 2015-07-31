-compile([{parse_transform, lager_transform},{export_all}]).

-define(DEFAULT_SERVICE_RETRY,2).
-define(DEFAULT_SERVICE_SLEEP,200).
-ifndef(LOGFORMAT).
-define(LOGFORMAT(Level,Formant,Args),
        lager:Level(Format,Args)).
-define(LOGMSG(Level,Format),
        lager:Level(Format)).
-endif.

-ifdef(debug).
-define(DEFAULT_SERVER_TIMEOUT,infinity).
-define(CALCULATED_SERVER_TIMEOUT(Timeout),Timeout).
-else.
-define(DEFAULT_SERVER_TIMEOUT,5000).
-define(CALCULATED_SERVER_TIMEOUT(Timeout),Timeout+?DEFAULT_SERVER_TIMEOUT).
-endif.
