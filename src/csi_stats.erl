%%%-------------------------------------------------------------------
%%% @author Zsolt Laky <zsolt.laky@erlang-solutions.com>
%%% @copyright (C) 2016, Erlang Solutions.
%%% @doc
%%% Common Service Interface application
%%% @end
%%% Created : 20 Jun 2015 by Erlang Solutions
%%%-------------------------------------------------------------------

-module(csi_stats).

%% ====================================================================
%% API functions
%% ====================================================================
-export([init_stats/0,
         response_time/8,
         req_per_sec/8
        ]).

init_stats() ->
    [{response_time, [{"last_nth_to_collect", 50},
                      {"normalize_to_nth", 40}]},
     {req_per_sec, [{"time_window", 50}
                   ]}
    ].
response_time(start, _Request, _R, Ref, _Params, _Tab, TempTab, TimeStamp) ->
    ets:insert(TempTab, {{response_time_first, Ref}, TimeStamp});

response_time(stop, Request, _R, Ref, Params, Tab, TempTab, TimeStamp) ->
    case ets:lookup(TempTab, {response_time_first, Ref}) of
        [{{response_time_first, Ref}, Value}] ->
            ResponseTime = TimeStamp - Value,
            ets:delete(TempTab, {response_time_first, Ref}),
            {NrOfReqs, ART, _Avg, MinRt, MaxRt} =
                case ets:lookup(Tab, {response_time, Request}) of
                    [] ->
                        {0, 0, 0, 10000000000000, 0};
                    [{{response_time, Request}, Values}] ->
                        Values
                end,
            {NormalizedART, NewNr} =
                case proplists:get_value("last_nth_to_collect",
                                         Params,
                                         50) =:= NrOfReqs of
                    true ->
                        NormalizeToNth = proplists:get_value("normalize_to_nth",
                                                             Params,
                                                             40),
                        {( ART / NrOfReqs ) * NormalizeToNth,
                         NormalizeToNth + 1};
                    _ ->
                        {ART, NrOfReqs + 1}
                end,
            AllRespTime = NormalizedART + ResponseTime,
            NewMinRt = min(MinRt, ResponseTime),
            NewMaxRt = max(MaxRt, ResponseTime),
            ets:insert(Tab, {{response_time, Request}, {NewNr,
                                                        AllRespTime,
                                                        AllRespTime/NewNr,
                                                        NewMinRt,
                                                        NewMaxRt}})
    end;

response_time(clean, _Request, _R, Ref, _Params, _Tab, TempTab, _TimeStamp) ->
    case Ref of
        undefined ->
            ok;
        Else ->
            ets:delete(TempTab, {response_time_first, Else})
    end.

req_per_sec(start, Request, _R, _Ref, Params, Tab, TempTab, TimeStamp) ->
    ReceivedTimestampList =
        case ets:lookup(TempTab, {req_per_sec_received_list, Request}) of
            [] ->
                [];
            [{{req_per_sec_received_list, Request}, Values}] ->
                Values
        end,
    RTL =
        case proplists:get_value("nr_of_timestamps",
                                 Params,
                                 50) =< length(ReceivedTimestampList) of
            true ->
                [TimeStamp | lists:droplast(ReceivedTimestampList)];
            _ ->
                [TimeStamp | ReceivedTimestampList]
        end,
    RPS =
        case length(RTL) >= 2 of
            true ->
                try
                    1000000 * length(RTL) / (TimeStamp - lists:last(RTL))
                catch
                    _:_ ->
                        99999999
                end;
            _ ->
                0
        end,
    ets:insert(TempTab, {{req_per_sec_received_list, Request}, RTL}),
    ets:insert(Tab, {{req_per_sec, Request}, RPS});

req_per_sec(stop, _Request, _R, _Ref, _Params, _Tab, _TempTab, _TimeStamp) ->
    ok;
req_per_sec(clean, _Request, _R, _Ref, _Params, _Tab, _TempTab, _TimeStamp) ->
    ok.

%% ====================================================================
%% Internal functions
%% ====================================================================