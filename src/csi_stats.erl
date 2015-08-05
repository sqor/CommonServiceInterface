%%%-------------------------------------------------------------------
%%% @author sqor <dev@sqor.com>
%%% @copyright (C) 2015, SQOR, Inc.
%%% @doc
%%% Statistical function for Common Service Interface application
%%% @end
%%% Created : 20 Jun 2015 by sqor <dev@sqor.com>
%%%-------------------------------------------------------------------

-module(csi_stats).

%% ====================================================================
%% API functions
%% ====================================================================
-export([response_time/7,
         req_per_sec/7
        ]).

response_time(start,_Request,_R,Ref,_Params,Tab,TimeStamp) ->
    ets:insert(Tab, {{response_time_first,Ref},TimeStamp});

response_time(stop,Request,_R,Ref,Params,Tab,TimeStamp) ->
    case ets:lookup(Tab, {response_time_first,Ref}) of
        [{{response_time_first,Ref},Value}] ->
            ResponseTime = TimeStamp - Value,
            ets:delete(Tab,{response_time_first,Ref}),
            {NrOfReqs,ART,_Avg,MinRt,MaxRt} = case ets:lookup(Tab, {response_time,Request}) of
                                                 [] ->
                                                     {0,0,0,1000000000000,0};
                                                 [{{response_time,Request},Values}] ->
                                                     Values
                                             end,
            {NormalizedART,NewNr} = 
                case proplists:get_value("last_nth_to_collect",
                                         Params, 
                                         10) =:= NrOfReqs of
                    true ->
                        NormalizeToNth = proplists:get_value("normalize_to_nth",
                                                             Params,
                                                             8),
                        {( ART / NrOfReqs ) * NormalizeToNth,NormalizeToNth + 1};
                    _ ->
                        {ART,NrOfReqs + 1}
                end,
            AllRespTime = NormalizedART + ResponseTime,
            NewMinRt = min(MinRt,ResponseTime),
            NewMaxRt = max(MaxRt,ResponseTime),
            ets:insert(Tab, {{response_time,Request},{NewNr,
                                                      AllRespTime,
                                                      AllRespTime/NewNr,
                                                      NewMinRt,
                                                      NewMaxRt}}),
            ets:delete(Tab, {response_time_first,Ref})
    end;

response_time(clean,_Request,_R,Ref,_Params,Tab,_TimeStamp) ->
    case Ref of
        undefined ->
            ok;
        Else ->
            ets:delete(Tab, {response_time_first,Else})
    end.

req_per_sec(start,Request,_R,_Ref,Params,Tab,TimeStamp) ->
    ReceivedTimestampList =
        case ets:lookup(Tab, {req_per_sec_received_list,Request}) of
            [] ->
                [];
            [{{req_per_sec_received_list,Request},Values}] ->
                Values
        end,
    RTL =
        case proplists:get_value("nr_of_timestamps",
                                 Params, 
                                 10) =< length(ReceivedTimestampList) of
            true ->
                [TimeStamp | lists:droplast(ReceivedTimestampList)];
            _ ->
                [TimeStamp | ReceivedTimestampList]
        end,
    RPS =
        case length(RTL) >= 2 of
            true ->
                try
                    10000000 * length(RTL) / (TimeStamp - lists:last(RTL))
                catch
                    _:_ ->
                        99999999
                end;
            _ ->
                0
        end,
    ets:insert(Tab, {{req_per_sec_received_list,Request},RTL}),
    ets:insert(Tab, {{req_per_sec,Request},RPS});

req_per_sec(stop,_Request,_R,_Ref,_Params,_Tab,_TimeStamp) -> ok;
req_per_sec(clean,_Request,_R,_Ref,_Params,_Tab,_TimeStamp) -> ok.

%% ====================================================================
%% Internal functions
%% ====================================================================


