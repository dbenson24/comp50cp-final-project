#!/usr/bin/env escript
%%! -sname client

main(_) ->
    {ok, P} = python:start([{python_path, "/h/chinst01/comp/50CP/final"}]),
    python:call(P, chat_client, main, []),
    io:format("~p~n", [P]),    
    ok.
