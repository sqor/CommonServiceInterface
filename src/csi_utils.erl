%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% common utilities
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------

-module(csi_utils).
-include("csi.hrl").

%% ====================================================================
%% API functions
%% ====================================================================
-export([watchdog_create/2,
         watchdog_loop/3]).


-export([add_elems_to_list/2,
         remove_elems_from_list/2,
         now_usec/0,
         timestamp_to_usec/1
        ]).

-export([call_server/2,
         call_server/3,
         call_server/4,
         call_server/5
        ]).

now_usec() ->
    timestamp_to_usec(os:timestamp()).

timestamp_to_usec({MegaSecs,Secs,MicroSecs}) ->
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

add_elems_to_list(ElemList,List) ->
    lists:foldl(fun (Elem, AccIn) ->
                         case lists:member(Elem, AccIn) of
                             true ->
                                 AccIn;
                             _ ->
                                 AccIn ++ [Elem]
                         end
                end,
                List,
                ElemList).

remove_elems_from_list(ElemList,List) ->
    lists:filter(fun (Elem) ->
                          lists:member(Elem, ElemList)
                 end,
                 List).

%% watchdog_create/2
%% @doc Spawns a process for watchdog_loop/3.
%% Returns the pid of the created watchdog process.
%% @end
-spec watchdog_create(MessageToSendWhenTimeout :: term(),
                      Timeout ::non_neg_integer) -> Result :: pid().
watchdog_create(Timeout,MessageToSendWhenTimeout) ->
    spawn(?MODULE, watchdog_loop, [self(), MessageToSendWhenTimeout, Timeout]).

-spec watchdog_loop(SenderPid :: pid(),
                    MessageToSendWhenTimeout :: term(),
                    Timeout :: non_neg_integer()) -> Result when
    Result :: pid().
%% @doc Loop a watchdog.
%% When Timeout reached,
%% sends the MessageToSendWhenTimeout message to it's creator (the process)
%% that called watchdog_create/2.
%% Can handle the following messages:
%%<ul>
%%<li>keepalive -> restarts the timer</li>
%%<li>stop -> stops the watchdog process</li>
%%<li>{reset_timeout,NewTimeout} -> restarts the watchdog with new Timeout.</li>
%%</ul>
%% @end
watchdog_loop(SenderPid,MessageToSendWhenTimeout, Timeout) ->
    receive
        {reset_timeout,NewTimeout} ->
            watchdog_loop(SenderPid,MessageToSendWhenTimeout,NewTimeout);
        keepalive ->
            watchdog_loop(SenderPid, MessageToSendWhenTimeout, Timeout);
        stop ->
            ok;
        WAFIT ->
            ?LOGFORMAT(error,"Watchdog for process ~p got an unexpected message:~p",[SenderPid,WAFIT]),
            watchdog_loop(SenderPid, MessageToSendWhenTimeout, Timeout)
    after
        Timeout ->
            SenderPid ! MessageToSendWhenTimeout
    end.

%% call_server/2
%% ====================================================================
%% @doc Call a gen server with a ServerName and Request
%% with timeout : {?DEFAULT_SERVER_TIMEOUT},
%%      retry   : {?DEFAULT_SERVICE_RETRY},
%%      sleep   : {?DEFAULT_SERVICE_SLEEP}
%% @end
-spec call_server(Server :: atom(),
                  Request :: term()) -> Result when
    Result :: term().
% ====================================================================
call_server(Server, Request) ->
    call_server(Server,
                Request,
                ?DEFAULT_SERVER_TIMEOUT,
                ?DEFAULT_SERVICE_RETRY,
                ?DEFAULT_SERVICE_SLEEP,
                ?DEFAULT_SERVICE_RETRY).

%% call_server/3
%% ====================================================================
%% @doc Call a gen server with a ServerName, Arguments, Retry count,
%% default timeout      : ?DEFAULT_SERVER_TIMEOUT,
%% default sleep time   : ?DEFAULT_SERVICE_SLEEP
%% @end
-spec call_server(Server :: atom(),
                  Request :: term(),
                  Retry :: integer()) -> Result when
    Result :: term().
% ====================================================================
call_server(Server, Request, Retry) ->
    call_server(Server,
                Request,
                ?DEFAULT_SERVER_TIMEOUT,
                Retry,
                ?DEFAULT_SERVICE_SLEEP,
                Retry).

%% call_server/4
%% ====================================================================
%% @doc Call a gen server with a ServerName, Arguments, Retry count,
%% Sleep time and default timeout : ?DEFAULT_SERVER_TIMEOUT
%% @end
-spec call_server(Server :: atom(),
                  Request :: term(),
                  Retry :: integer(),
                  Sleep :: integer()) -> Result when
    Result :: term().
% ====================================================================
call_server(Server, Request, Retry, Sleep) ->
    call_server(Server,
                Request,
                ?DEFAULT_SERVER_TIMEOUT,
                Retry,
                Sleep,
                Retry).

%% call_server/5
%% ====================================================================
%% @doc Call a gen server with a ServerName, Arguments,
%% Retry count, Sleep time and Timeout
%% @end
-spec call_server(Server :: atom(),
                  Request :: term(),
                  Retry :: integer(),
                  Sleep :: integer(),
                  Timeout :: integer() | infinity) -> Result when
    Result :: term().
% ====================================================================
call_server(Server, Request, Retry, Sleep, Timeout) ->
    call_server(Server,
                Request,
                Timeout,
                Retry,
                Sleep,
                Retry).

%% ====================================================================
%% Internal functions
%% ====================================================================


%% call_server/6
%% ====================================================================
%% @doc <a>Call a gen server with a ServerName, Arguments, Retry count,
%% Sleep time and Timeout</a>
%% @end
-spec call_server(ServerName :: atom(),
                  Request :: term(),
                  Timeout:: integer() | infinity,
                  RetryCount :: integer(),
                  Sleep :: integer(),
                  Count :: integer() | infinity) -> Result when
    Result :: term().

% ====================================================================
call_server(Server,Request,Timeout,RetryCount,Sleep,Count) ->
    try
        gen_server:call(Server, Request ,Timeout)
    catch
        exit:{timeout,Location} ->
            {error,{timeout,Location}};
        A:B ->
            ?LOGFORMAT(error,"Calling server ~p raised an exception:~p:~p",[Server,A,B]),
            case Count of
                0 ->
                    try_service_discovery(Server,Request);
                _ ->
                    timer:sleep(Sleep),
                    call_server(Server,
                                Request,
                                Timeout,
                                RetryCount,
                                Sleep,
                                Count - 1)
            end

    end.
%% ====================================================================
%% Internal functions
%% ====================================================================

%% @TODO implement calling the server using service discovery
try_service_discovery(_Server,_Request) ->
    {error,noserver}.

