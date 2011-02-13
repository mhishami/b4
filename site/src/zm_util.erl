-module(zm_util).
-author('Hisham <mhishami@gmail.com').
-compile(export_all).


datehour() ->
    datehour(now()).

datehour(Now) ->
    {{Y, M, D}, {H, _, _}} = calendar:now_to_universal_time(Now),
    {Y, M, D, H}.

datetime() ->
    erlang:list_to_binary(datetime2()).

datetime2() ->
    {{Year,Month,Day},{Hour,Min,Sec}} = erlang:localtime(),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B",
                   [Year, Month, Day, Hour, Min, Sec]).

url_key(Rec, {_Record, List}) ->
    Key = proplists:get_value(key, List),
    wf:f("{~p, ~p}", [Rec, Key]).

