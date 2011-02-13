
-record(b4_user, {
        username, 
        password, 
        mobile_no, 
        email
}).

-record(b4_profile, {
        username, 
        first, 
        last, 
        company, 
        country, 
        postcode
}).

-record(b4_todo_list, {
        id, 
        text, 
        tstamp, 
        total, 
        done
}).

-record(b4_todo_item, {
        id, 
        text, 
        due, 
        notify
}).


