
-module(todolist, [Model, Bucket]).
-extends(zm_model).

-compile(export_all).
-define(BUCKET, <<"b4_todolist">>).

new() ->
    {todolist, todolist, <<"b4_todolist">>}.

to_list(User, Text) ->
    {todolist, [
        {user, User},
        {text, Text},
        {datetime, zm_util:datetime()}
    ]}.

get_by_user(Username) ->
    %M = {map, {jsfun, <<"Riak.mapValuesJson">>}, undefined, true},
    M = {map, {jsanon, <<"
            function(value, keyData, arg) {
                var obj = Riak.mapValuesJson(value)[0];
                if (obj.user === arg) {
                    return [obj];
                }
                return [];
            }
            ">>}, Username, true},
    T = todolist:new(),
    case T:bucket_query([M]) of
        {error, _}       -> [];
        {ok, [{_, Rec}]} -> Rec
    end.
    
    %Json = [mochijson2:encode(X) || X <- Rec],
    %Data = [T:from_json(X) || X <- Json],
    %[format_data(X) || X <- Data].

format_data({todolist, Data}) ->
    Data.


 
