%% -*- mode: nitrogen -*-
-module (login).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Login".

body() -> 
    #container_12 { body=[
        #grid_6 { class=zmborder, alpha=true, prefix=3, suffix=3, omega=true, body=login() }
    ]}.

login() ->
    Body = [
        #panel { style="margin: 20px;", body=[
            common:logo("/"),
            #h1 { text="Login" },
            #label { text="Username" },
            #textbox { id=usernameText, next=password },
            #br{},
            #label { text="Password" },
            #password { id=passwordText, next=loginButton },
            #br{},
            #button { id=loginButton, text="Login", postback=auth }
        ]}
    ],
    wf:wire(loginButton, usernameText, #validate { validators = [
                #is_required { text="Username is required" }
            ]}),
    wf:wire(loginButton, passwordText, #validate { validators = [
                #is_required { text="Password is required" }
            ]}),
    Body.

event(auth) ->
    Username = wf:q(usernameText),
    Password = common:hash2(wf:q(passwordText)),

    %% read user record from DB
    U = user:new(),
    case U:read(Username) of
        {user, Rec} ->
            wf:info(wf:f("Got user record of ~p", [Rec])),
            case proplists:get_value(password, Rec, undefined) of
                undefined ->
                    wf:redirect("/");
                Val ->
                    case Val =:= Password of
                        true ->
                            % ok, password is good
                            wf:user(Username),
                            wf:redirect_from_login("/dashboard");
                        _ ->
                            wf:redirect("/")
                    end
            end;
        _ ->
            wf:redirect("/")
    end,
    ok.


