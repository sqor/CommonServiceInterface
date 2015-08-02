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
-export([response_time/6
        ]).


response_time(start,_Request,_R,Ref,_Params,Tab) ->
    ets:insert(Tab, {{response_time_first,Ref},csi_utils:now_usec()});

response_time(stop,Request,_R,Ref,Params,Tab) ->
    case ets:lookup(Tab, {response_time_first,Ref}) of
        [{{response_time_first,Ref},Value}] ->
            ResponseTime = csi_utils:now_usec() - Value,
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
                                                      NewMaxRt}})
    end;

response_time(clean,_Request,_R,Ref,_Params,Tab) ->
    case Ref of
        undefined ->
            ok;
        Else ->
            ets:delete(Tab, {response_time_first,Else})
    end.


%% ====================================================================
%% Internal functions
%% ====================================================================


