-module(app).

-define(SERVICE_NAME, app_service).
-define(SERVICE_MODULE, app_service).

%% ====================================================================
%% API functions
%% ====================================================================

-export([process_foo/1,
         process_too_long/1,
         process_crashing/1]).

process_foo(Atom) -> csi:call_p(?SERVICE_NAME, process_foo, [Atom]).
process_too_long(Atom) -> csi:call_p(?SERVICE_NAME, process_too_long, [Atom]).
process_crashing(Atom) -> csi:call_p(?SERVICE_NAME, process_crashing, [Atom]).
