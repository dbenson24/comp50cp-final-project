#!/usr/bin/env escript


main(_) ->
    code:load_abs("ebin/userserver"),
    %chat:start_link(),
    userserver:start_link(),
    io:format("server started"),
    receive a -> a end.
