%% -*- mode: nitrogen -*-
-module (common).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").


main() ->
    case wf:user() /= undefined of
        true -> main_authorized();
        false -> wf:redirect_to_login("/login")
    end.

main_authorized() -> #template { file="./site/templates/bare.html" }.

logo(Url) -> 
    [
        #panel { class=zmlogo, body=[
            #link { url=Url, body=[#image { image="/images/logo.png" }]},
            #flash {}
        ]}
    ].

footer() ->
    [
        #panel { class=zmfooter, body=[
            #p { body=["&copy; 2010 Hisham & Co. All rights reserved"] }
        ]}
    ].

side_panel() ->
    [
        #panel { class=zmpanel, body=[
            #span { text="Welcome " ++ wf:user() ++ "." },
            #hr{},
            #list { body=[
                #listitem { body=#link { text="logout", postback=logout, delegate=common }},
                #listitem { body=#link { text="home", url="/dashboard" }},
                #listitem { body=#link { text="settings", url="/settings" }}
            ]}
        ]}
    ].

hash2(Data) ->
    H = erlang:phash2(Data),
    base64:encode(<<H:32>>).

event(logout) ->
    wf:clear_session(),
    wf:redirect("/"),
    ok;

event(Any) -> 
    wf:flash(wf:f("Common: Uncaught event! - ~p", [Any])),
    ok.


