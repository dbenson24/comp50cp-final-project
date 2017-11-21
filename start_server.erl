#!/usr/bin/env escript


main(_) ->
    c(user),
    %chat:start_link(),
    user:start_link(),
    io:format("server started"),
    receive a -> a end.
