%% -*- mode: nitrogen -*-
-module (index).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

main() -> #template { file="./site/templates/bare.html" }.

title() -> "Welcome!".

body() ->
    #container_12 { class=zmborder, body=[
        #grid_12 { class=showborder, body=common:logo("/") },
        #grid_clear {},

        #grid_12 { class=showborder, alpha=true, omega=true, body=action() },
        #grid_clear {},

        #grid_5 { class=showborder, alpha=true, body=adv() },
        #grid_7 { class=showborder, omega=true, body=rotate() },
        #grid_clear {},

        #grid_12 { class=showborder, alpha=true, omega=true, body=common:footer() }
    ]}.


action() ->
    [
        #panel { style="padding: 10px;", body=[
            #link { url="/login", text="Login to my account" },
            #span { text="  " },
            #link { url="/register", text="Register" }
        ]}
    ].

adv() ->
    [
        #panel { style="margin: 10px;", body=[
            #h3 { text="Some Advert" },
            #p { class="adv", body=[
                    "b414get is the simplest way of getting remembered in doing things..."
            ]},
            #p { body=[
                " Don't forget things anymore, the easy way..  "
            ]}
        ]}
    ].

rotate() ->
    [
        #panel { style="padding: 10px; border: 2px solid #ccc; height: 250px;", body=[
            #p { body=[
                " Some rotating pictures "
            ]}
        ]}
    ].


