#!/usr/bin/env escript
%%! -sname main


main(_) ->
    userserver:start_link(),
    nodemanager:start_link(node()),
    io:format("server started on node ~p\n", [node()]),
    receive stop -> ok end.
