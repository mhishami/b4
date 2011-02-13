
-module(user, [Model, Bucket]).
-extends(zm_model).

-export([new/0, to_user/4]).

new() ->
    {user, user, <<"b4_user">>}.

to_user(Username, Password, Email, MobileNo) ->
    {user, [
        {key, Username},
        {username, Username},
        {password, Password},
        {email, Email},
        {mobile_no, MobileNo}
    ]}.

