%% -*- mode: nitrogen -*-
-module (settings).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").


main() ->
    case wf:user() /= undefined of
        true -> main_authorized();
        false -> wf:redirect_to_login("/login")
    end.

main_authorized() -> #template { file="./site/templates/bare.html" }.

title() -> "settings".

body() -> 
    #container_12 { class=zmborder, body=[
        #grid_12 { class=showborder, omega=true, body=common:logo("/dashboard") },
        #grid_clear {},

        #grid_9 { class=showborder, alpha=true, body=content() },
        #grid_3 { class=showborder, omega=true, body=common:side_panel() },
        #grid_clear {},

        #grid_12 { class=showborder, alpha=true, body=common:footer() }
    ]}.


content() ->
    [
        #panel { body=[
            #panel { id=tabs1, class=zmsettings, body=[
                get_profile()
            ]},
            #p{},
            #panel { id=tabs2, class=zmsettings, body=[
                change_passwd()
            ]}
        ]}
    ].

get_profile() ->
    [
        #panel { body=[
            #h2 { text="User Profile" },
            #hr{},
            #br{},

            #label { text="Mobile No." }, 
            #textbox { class=zm_small, text="+60196622165" },
            #br{},

            #label { text="Email Address" }, 
            #textbox { class=zm_small, text="mhishami@gmail.com" },
            #p{},

            #button { text="Update", postback=profile }
        ]}
    ].

change_passwd() ->
    [
        #panel { body=[
            #h3 { text="Change Password" },
            #hr{},
            #br{},

            #label { text="Old Password" },
            #password { id=oldPasswd, class=zm_small, next=newPasswd },
            #br{},

            #label { text="New Password" },
            #password { id=newPasswd, class=zm_small, next=newPasswdAgain },
            #br{},

            #label { text="New Password (Again)" },
            #password { id=newPasswdAgain, class=zm_small, next=okButtonPasswd },
            #p{},

            #button { id=okButtonPasswd, text="Update", postback=change_passwd }
        ]}
    ].

event(Any) -> 
    wf:flash(wf:f("Settings: Uncaught event! - ~p", [Any])),
    ok.


