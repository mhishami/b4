%% @author OJ Reeves <oj@buffered.io>
%% @copyright 2010 OJ Reeves

%% @doc Functions which wrap up the communication with the Riak cluster
%%      plus a few other helper functions.

-module(zm_riak).
-include_lib("riakc/include/riakc_obj.hrl").

%% Riak exports
-export([connect/0, connect/1, create/3, create/4, fetch/3, update/2, 
         delete/3, get_value/1, save/2]).

%% object relationship
-export([add_link/4, remove_link/4]).

%% query
-export([bucket_query/3, mapred_query/3]).

%% helper functions for generating unique keys.
-export([new_key/0, new_key/1]).

%% ----------------------------------------------- Exported Functions

connect() ->
    connect({"127.0.0.1", 8080}).

%% @spec connect(connection_info()) -> pid()
%% @doc Create a connection to the specified Riak cluster and
%%      return the Pid associated with the new connection.
connect({IP, Port}) ->
    {ok, RiakPid} = riakc_pb_socket:start_link(IP, Port),
    RiakPid.

%% @spec create(binary, binary, json) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      assumes that the data passed in is JSON and sets
%%      the MIME type to "application/json" for you.
create(Bucket, Key, JsonData) ->
    create(Bucket, Key, JsonData, "application/json").

%% @spec create(binary, binary, term(), string) -> riakc_obj()
%% @doc Create a new instance of a riak object using the
%%      parameters given. The riak object can then be
%%      persisted to a Riak node/cluster. This overload
%%      takes arbitrary data and requires the user to
%%      specify the mime type of the data that is being
%%      stored.
create(Bucket, Key, Item, MimeType) ->
    riakc_obj:new(Bucket, Key, Item, MimeType).

%% @spec fetch(pid(), binary, binary) -> riakc_obj()
%% @doc Fetches a riakc object from a Riak node/cluster
%%      using the connection given.
fetch(RiakPid, Bucket, Key) ->
    riakc_pb_socket:get(RiakPid, Bucket, Key).

%% @spec update(riakc_obj(), term()) -> riakc_obj()
%% @doc Updates the stored value for a riakc object with
%%      the new one specified.
update(RiakObj, NewValue) ->
    riakc_obj:update_value(RiakObj, NewValue).

%% @spec delete(pid(), term(), term()) -> ok.
%% @doc delete the entry in the bucket by key
%%
delete(RiakPid, Bucket, Key) ->
    riakc_pb_socket:delete(RiakPid, Bucket, Key).

%% @spec get_value(riakc_obj()) -> term()
%% @doc Retrieves the stored value from within the riakc
%%      object.
get_value(RiakObj) ->
    riakc_obj:get_value(RiakObj).

%% @spec save(pid(), riakc_obj()) -> {ok, riakc_obj()} | {error | Reason}
%% @doc Saves the given riak object to the specified Riak node/cluster.
save(RiakPid, RiakObj) ->
    riakc_pb_socket:put(RiakPid, RiakObj).

add_link(RiakPid, Bucket, Key, LinkData={{_OtherBucket, _OtherKey}, _Link}) ->
    {ok, O} = fetch(RiakPid, Bucket, Key),
    M1 = riakc_obj:get_metadata(O),
    % see if it has any link
    M2 = case dict:find(?MD_LINKS, M1) of 
             {ok, L1} -> 
                 dict:store(?MD_LINKS, [LinkData|L1], M1);
             _ -> 
                 dict:store(?MD_LINKS, [LinkData], M1)
         end,
    O2 = riakc_obj:update_metadata(O, M2),
    save(RiakPid, O2).

remove_link(RiakPid, Bucket, Key, LinkData={{_OtherBucket, _OtherKey}, _Link}) ->
    {ok, O} = fetch(RiakPid, Bucket, Key),
    M1 = riakc_obj:get_metadata(O),
    M2 = case dict:find(?MD_LINKS, M1) of
             {ok, L1} ->
                 L2 = lists:delete(LinkData, L1),
                 dict:store(?MD_LINKS, L2, M1);
             _ ->
                 M1
         end,
    O2 = riakc_obj:update_metadata(O, M2),
    save(RiakPid, O2).

bucket_query(RiakPid, Bucket, MapRed) ->
    Res = riakc_pb_socket:mapred_bucket(RiakPid, Bucket, MapRed),
    Res.

mapred_query(RiakPid, Bucket, MapRed) ->
    Res = riakc_pb_socket:mapred(RiakPid, Bucket, MapRed),
    Res.

%% @spec new_key() -> key()
%% @doc Generate an close-to-unique key that can be used to identify
%%      an object in riak. This implementation is blatantly borrowed
%%      (purloined) from the wriaki source (thanks basho!)
new_key() ->
    {{Yr, Mo, Dy}, {Hr, Mn, Sc}} = erlang:universaltime(),
    {_, _, Now} = now(),
    new_key([Yr, Mo, Dy, Hr, Mn, Sc, node(), Now]).

%% @spec new_key(list()) -> key()
%% @doc Generate an close-to-unique key that can be used to identify
%%      an object in riak using the given list parameter as the stuff
%%      to hash.
new_key(List) ->
    Hash = erlang:phash2(List),
    base64:encode(<<Hash:32>>).


