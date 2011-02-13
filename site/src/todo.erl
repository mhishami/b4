%% -*- mode: nitrogen -*-
-module (todo).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").
-include("records.hrl").

-record(todolist, {id, text, due, notify}).

main() ->
    case wf:user() /= undefined of
        true -> main_authorized();
        false -> wf:redirect_to_login("/login")
    end.

main_authorized() -> #template { file="./site/templates/bare.html" }.

title() -> "todo list".

body() -> 
    #container_12 { class=zmborder, body=[
        #grid_12 { class=showborder, alpha=true, omega=true, body=common:logo("/dashboard") },
        #grid_clear {},

        #grid_9 { class=showborder, alpha=true, body=content() },
        #grid_3 { class=showborder, omega=true, body=common:side_panel() },
        #grid_clear {},

        #grid_12 { class=showborder, alpha=true, omega=true, body=common:footer() }
    ]}.

get_data() ->
    [
        {todolist, {todolist, <<"1111">>}, <<"Bring sweater">>, <<"18-10-2010T16:35">>, <<"yes">>},
        {todolist, {todolist, <<"1212">>}, <<"Buy new PC">>, <<"18-10-2010T16:35">>, <<"yes">>},
        {todolist, {todolist, <<"1313">>}, <<"Get business proposal">>, <<"18-10-2010T16:35">>, <<"yes">>} 
    ].

get_map() ->
    #todolist { 
        id=todoText@postback,
        text=todoText@text,
        due=todoDue@text,
        notify=todoNotify@text
    }.

content() ->
    Data = get_data(),
    Map = get_map(),
    [
        #panel { class=zmtodoitem, body=[
            #h2 { class=zmlist, text=wf:session(todoList) },
            #button { text="My Dashboard", postback=dashboard },
            #button { id=newTodoItem, text="New ToDo Item", postback=new_item },
            #button { text="Remove This List", postback=rm_list },
            #hr{},
            #list { id=todoListMain, class=zmtodolist, body=[
                #bind { id=todoListItemBind, data=Data, map=Map, body=[
                    #listitem { id=todoListItem, body=[
                        #checkbox { id=todoText }
                    ]},
                    #hr{}
                ]}
            ]},
            #button { text="Done", postback=delete }

        ]}
    ].

new_item() ->
    FlashId = wf:temp_id(),
    TodoItem = wf:temp_id(),
    DueDate = wf:temp_id(),
    Notify = wf:temp_id(),
    UI = [
        #panel { style="padding: 10px;", body=[
            #label { text="Enter list item:" },
            #textbox { class=zm_large, id=TodoItem },
            #label { text="Due date:" },
            #datepicker_textbox { id=DueDate },
            #br{},
            #checkbox { id=Notify, text="Notify me over SMS" },
            #p{},
            #button { id=addItem, text="Ok", postback={flash, FlashId, TodoItem, DueDate, Notify} },
            #button { id=cancelAddItem, text="Cancel", postback={flash, cancel, FlashId} }
        ]}
    ],
    wf:flash(FlashId, UI).

event(dashboard) ->
    wf:redirect("/dashboard");

event(new_item) ->
    wf:wire(newTodoItem, #hide { effect=blind, speed=100 }),
    new_item();

event({flash, cancel, FlashId}) ->
    wf:wire(FlashId, #hide { effect=blind, speed=100 }),
    wf:wire(newTodoItem, #show { effect=blind, speed=100 }),
    ok;

event(delete) ->
    wf:flash(wf:f("Deleting ~p...", [wf:state(delList)]));

event({todolist, Id}) ->
    List = case wf:state(delList) of 
               undefined -> [];
               Any -> Any
           end,
    case lists:member(Id, List) of
        true  -> wf:state(delList, lists:delete(Id, List));
        false -> wf:state(delList, [Id|List])
    end,
    ok;

event(Any) -> 
    wf:flash(wf:f("Todo: Uncaught event! - ~p", [Any])).

