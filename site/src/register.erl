%% -*- mode: nitrogen -*-
-module (register).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Login".

body() -> 
    #container_12 { body=[
        #grid_8 { class=zmborder, alpha=true, prefix=3, suffix=3, omega=true, body=content() }
    ]}.

content() ->
    Body = [
        #panel { class=zmpad, body=[
            common:logo("/"),
            #h1 { text="Register" }
        ]},
        #panel { class=zmregister, body=[

            #label { text="Username" },
            #textbox { id=usernameText, next=passwordText },
            #br{},

            #label { text="Password" },
            #password { id=passwordText, next=loginButton },
            #br{},

            #label { text="Email Address" },
            #textbox { id=emailAddress, next=mobileNUmber },
            #br{},

            #label { text="Mobile Number" },
            #textbox { id=mobileNumber, next=loginButton },
            #br{},

            #button { id=loginButton, text="Register", postback=auth },
            #span { text=" | " },
            #link { text="Login", url="/login" },
            #span { text=" | " },
            #link { text="Not now!", url="/" }
        ]}
    ],
    wf:wire(loginButton, usernameText, #validate { validators = [
                #is_required { text="Username is required" }
            ]}),
    wf:wire(loginButton, passwordText, #validate { validators = [
                #is_required { text="Password is required" }
            ]}),
    wf:wire(loginButton, emailAddress, #validate { validators = [
                #is_required { text="Email address is required" }
            ]}),
    wf:wire(loginButton, mobileNumber, #validate { validators = [
                #is_required { text="Mobile number is required" }
            ]}),
    Body.

event(auth) ->
    Username = wf:q(usernameText),
    Password = common:hash2(wf:q(passwordText)),
    MobileNo = wf:q(mobileNumber),
    EmailAddr = wf:q(emailAddress),

    % create user
    User = user:new(),
    Rec = User:to_user(Username, Password, EmailAddr, MobileNo),
    User:create(Rec),

    wf:user(Username),
    wf:redirect_from_login("/dashboard"),
    ok.


