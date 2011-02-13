%% -*- mode: nitrogen -*-
-module (admin).
-compile(export_all).
-include_lib("nitrogen/include/wf.hrl").

change_passwd(OldPass, NewPass) ->
    %wf:flash(wf:f("NewPass= ~p, OldPass= ~p", [NewPass, OldPass])),
    ok.

