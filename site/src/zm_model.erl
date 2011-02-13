
-module(zm_model, [Model, Bucket]).

-export([to_json/1, from_json/1]). 
-export([get_model/0, get_bucket/0]).

-export([create/1, create/2, read/1, read/2, update/1, update/2, delete/1, delete/2]).
-export([mapred_query/2, mapred_query/3, bucket_query/1, bucket_query/2]).


to_json({Model, Data}) ->
    to_json_internal(Data).

get_model() -> Model.
get_bucket() -> Bucket.

from_json(JsonData) ->
    from_json_internal(JsonData).

create(Record) ->
    create(zm_riak:connect(), Record).

create(RiakPid, Record={Model, Data}) ->
    case proplists:get_value(key, Data, undefined) of 
        undefined -> 
            Key = zm_riak:new_key(), 
            NewData = [{key, Key} | Data], 
            RiakObj = zm_riak:create(Bucket, Key, to_json_internal(NewData)), 
            ok = zm_riak:save(RiakPid, RiakObj), 
            {Model, NewData}; 
        Key ->
            RiakObj = zm_riak:create(Bucket, Key, to_json_internal(Data)), 
            ok = zm_riak:save(RiakPid, RiakObj), 
            Record 
    end.

read(Key) ->
    read(zm_riak:connect(), Key).

read(RiakPid, Key) ->
    case zm_riak:fetch(RiakPid, Bucket, Key) of
        {ok, RiakObj} -> 
            JsonData = zm_riak:get_value(RiakObj), 
            from_json_internal(JsonData);
        _ ->
            {not_found}
    end.


update(Record) ->
    update(zm_riak:connect(), Record).

update(RiakPid, Record={Model, Data}) ->
    case proplists:get_value(key, Data, undefined) of 
        undefined ->
            {no_key_defined};
        ExistingKey -> 
            RiakObj = zm_riak:fetch(RiakPid, Bucket, ExistingKey), 
            NewRiakObj = zm_riak:update(RiakObj, to_json_internal(Data)), 
            ok = zm_riak:save(RiakPid, NewRiakObj), 
            Record
    end.

delete(Key) ->
    delete(zm_riak:connect(), Key).

delete(RiakPid, Key) ->
    zm_riak:delete(RiakPid, Bucket, Key).
 

%% ----------------------------------------------------------------------------
%% Query functions
%% ----------------------------------------------------------------------------

mapred_query(Bucket, MapRed) ->
    mapred_query(zm_riak:connect(), Bucket, MapRed).

mapred_query(RiakPid, Bucket, MapRed) ->
    zm_riak:mapred_query(RiakPid, Bucket, MapRed).

%% @spec bucket_query(MapRed) -> term() | MapRed :: lists().
%% @doc Do bucket query based on the map reduce parameters given
%%
bucket_query(MapRed) ->
    bucket_query(zm_riak:connect(), MapRed).

%% @spec bucket_query(RiakPid, MapRed) -> term() | RiakPid :: pid(),
%%                                                 MapRed :: lists().
%%
%% @doc Do bucket query based on the map reduce parameters given
%%
bucket_query(RiakPid, MapRed) ->
    %io:format("MapRed = ~p, Bucket = ~p", [MapRed, Bucket]),
    zm_riak:bucket_query(RiakPid, Bucket, MapRed).


%% ----------------------------------------------------------------------------
%% Internal functions
%% ----------------------------------------------------------------------------
to_json_internal(Data) ->
    zm_json:to_json(Data, fun is_string/1).

from_json_internal(JsonData) ->
    {Model, zm_json:from_json(JsonData, fun is_string/1)}.

is_string(_) -> false.



