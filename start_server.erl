#!/usr/bin/env escript
%%! -sname main


main(_) ->
    code:load_abs("ebin/userserver"),
    %chat:start_link(),
    userserver:start_link(),
    io:format("server started on node ~p\n", [node()]),
    receive stop -> ok end.
