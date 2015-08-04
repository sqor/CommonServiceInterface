-module(em).

-define(SERVICE_NAME,em_service).
-define(SERVICE_MODULE,em_service).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0,
         start_link/0,
         stop/0]).

-export([process_foo/1,
         process_too_long/1,
         process_crashing/1]).


start() -> csi:start(?SERVICE_NAME,?SERVICE_MODULE).
start_link() -> csi:start_link(?SERVICE_NAME,?SERVICE_MODULE).

stop() -> csi:stop(?SERVICE_NAME).

process_foo(Atom) -> csi:call_p(?SERVICE_NAME,process_foo,[From]).
process_too_long(Atom) -> csi:call_p(?SERVICE_NAME,process_too_long,[From]).
process_crashing(Atom) -> csi:call_p(?SERVICE_NAME,process_crashing,[From]).
