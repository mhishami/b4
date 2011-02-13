
-module(linkw).
-include_lib("nitrogen/include/wf.hrl").
-compile(export_all).

-define(BUCKET, <<"b4_user">>).
-define(HOST, "127.0.0.1").
-define(PORT, 8080).

populate() ->
    Pid = zm_riak:connect(),
    F = fun(X) ->
            V = erlang:list_to_binary(erlang:integer_to_list(X)),
            O = riakc_obj:new(<<"count">>, V),
            riakc_pb_socket:put(Pid, riakc_obj:update_value(O, V, "application/json"))
        end,
    lists:foreach(F, lists:seq(1,1000)).

search() ->
    Pid = zm_riak:connect(),
    M = {map, {jsfun, <<"Riak.mapValuesJson">>}, undefined, true},
    riakc_pb_socket:mapred(Pid, [{<<"count">>, <<"2">>},
                                 {<<"count">>, <<"3">>}, 
                                 {<<"count">>, <<"4">>}], 
                                [M]).


sum() ->
    Pid = zm_riak:connect(),
    M = {map, {jsfun, <<"Riak.mapValuesJson">>}, undefined, false},
    R = {reduce, {jsfun, <<"Riak.reduceSum">>}, undefined, true},
    Res = riakc_pb_socket:mapred(Pid, [{<<"count">>, <<"2">>},
                                       {<<"count">>, <<"3">>}, 
                                       {<<"count">>, <<"4">>}], 
                                       %{<<"count">>, <<"5">>}, 
                                       %{<<"count">>, <<"6">>}, 
                                       %{<<"count">>, <<"8">>}], 
                                      [M, R]),
    {ok, [{_Row, [Val]}]} = Res,
    io:format("~n*** Sum of all: ~p~n", [Val]).

sum_all() ->
    Pid = zm_riak:connect(),
    M = {map, {jsfun, <<"Riak.mapValuesJson">>}, undefined, false},
    R = {reduce, {jsfun, <<"Riak.reduceSum">>}, undefined, true},

    Res = riakc_pb_socket:mapred_bucket(Pid, <<"count">>, [M, R]),
    {ok, [{_Row, [Val]}]} = Res,
    io:format("~n*** Sum of all in bucket: ~p~n", [Val]).

