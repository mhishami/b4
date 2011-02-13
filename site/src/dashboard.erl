%% -*- mode: nitrogen -*-
-module (dashboard).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

-record(todo, {id, text, tstamp, total, done}).

main() ->
    case wf:user() /= undefined of
        true -> main_authorized();
        false -> wf:redirect_to_login("/login")
    end.

main_authorized() -> 
    #template { file="./site/templates/bare.html" }.

title() -> "dashboard".

body() -> 
    #container_12 { class=zmborder, body=[
        #grid_12 { class=showborder, omega=true, body=common:logo("/dashboard") },
        #grid_clear {},

        #grid_9 { class=showborder, alpha=true, body=content() },
        #grid_3 { class=showborder, omega=true, body=common:side_panel() },
        #grid_clear {},

        #grid_12 { class=showborder, alpha=true, body=common:footer() }
    ]}.

get_data() ->
    [
        {todo, {todo, <<"1111">>}, <<"Riyadh Trip">>, <<"18-10-2010T16:35">>, "2", <<"1">>},
        {todo, {todo, <<"1212">>}, <<"Groceries">>, <<"18-10-2010T16:35">>, <<"5">>, <<"2">>},
        {todo, {todo, <<"1313">>}, <<"Moving out from apartment">>, <<"18-10-2010T16:35">>, <<"5">>, <<"3">>}
    ].

get_data2() ->
    T = todolist:new(),
    Todos = T:get_by_user("hisham"),
    Todos.

get_map() ->
    #todo { 
        id=todo@postback,
        text=todo@text,
        tstamp=tstamp@text,
        total=total@text,
        done=done@text
    }.

content() ->
    Data = get_data(),
    Map = get_map(),
    [
        #panel { id=todoPanel, class=zmtodo, body=[
            #h2 { text="My ToDo Lists" },
            #button { id=todoButton, text="New ToDo List", postback=new_list },
            #hr{},
            #list { id=todoListMain, class=zmtodolist, body=[
                #bind { id=todoListBind, data=Data, map=Map, body=[
                    #listitem { id=todoListEntry, body=[
                        #checkbox { id=deleted },
                        #link { id=todo }, 
                        #span { class=zminline, id=done},
                        #span { class=zminline, text=" done, " },
                        #span { class=zminline, id=total },
                        #span { class=zminline, text=" total" }
                    ]},
                    #hr{}
                ]}
            ]},
            #button { text="Delete", postback=delete }
        ]}
    ].

new_list() ->
    FlashId = wf:temp_id(),
    ListId = wf:temp_id(),
    AddButton = wf:temp_id(),
    wf:wire(todoButton, #hide { effect=blind, speed=30 }),
    UI = [
        #panel { style="padding: 10px;", body=[
            #label { text="Enter list name" },
            #textbox { id=ListId, class=zm_large, next=addListButton },
            #br{},
            #button { id=AddButton, text="Ok", postback={flash, list, FlashId, ListId} },
            #button { text="Cancel", postback={flash, cancel, FlashId} }
        ]}
    ],
    wf:flash(FlashId, UI),
    ok.

%event({todo, _Id, Text, foo}) ->
%    wf:session(todoList, Text),
%    wf:redirect("/todo"),
%    ok;

event({flash, cancel, FlashId}) ->
    wf:wire(FlashId, #hide { effect=blind, speed=100 }),
    wf:wire(todoButton, #show { effect=blind, speed=30 }),
    ok;

event({flash, list, FlashId, ListId}) ->
    case wf:q(ListId) of
        "" -> 
            wf:wire(#alert { text="List cannot be empty!" });
        _  ->
            wf:wire(FlashId, #hide { effect=blind, speed=100 }),
            wf:wire(todoButton, #show { effect=blind, speed=30 }),
            %%
            %% add to record
            %%
            L = todolist:new(),
            L2 = L:to_list(wf:user(), wf:q(ListId)),
            wf:info(wf:f("L2 = ~p", [L2])),
            L3 = L:create(L2),
            wf:info(wf:f("L3 = ~p", [L3])),
            {_Rec, Key} = zm_util:url_key(todo, L3),
            %UrlId = wf:f("{~p, ~p}", [Rec, Key]),

            wf:insert_bottom(todoListMain, 
                #listitem { body=[
                    #checkbox { id=deleted },
                    #link { id={todo, Key}, text=wf:q(ListId) },
                    #span { class=zminline, text="Newly added" }
                ]}
            ),
            wf:insert_bottom(todoListMain, #hr{})
    end,
    ok;

event(new_list) ->
    new_list(),
    ok;

event(delete) ->
    wf:flash(wf:f("Deleted entry = ~p", [wf:qs(deleted)])),
    ok;

event(Any) -> 
    wf:flash(wf:f("Dashboard: Uncaught event! - ~p", [Any])),
    ok.

