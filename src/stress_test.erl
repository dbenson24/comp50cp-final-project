-module(stress_test).

-export([for_i/2, subscribe/2, spawn_clients/2, blast_messages/1, start_blasts/0]).

for_i(0, _F) ->
    ok;
for_i(I, F) ->
    F(I),
    for_i(I-1, F).

subscribe(UserServer, UserName) ->
    {ok, _} = clientserver:start_link(UserServer, UserName),
    ok = clientserver:join_room("test_room1", UserName),
    ok = clientserver:join_room("test_room2", UserName),
    ok = clientserver:join_room("test_room3", UserName),
    ok = clientserver:join_room("test_room4", UserName),
    {ok, _Handler} = clientserver:register_handler(fun (Room, _FromUser, Message) -> io:format("user: ~p in room: ~p got ~p~n", [UserName, Room, Message]) end, UserName),
    ok.

spawn_clients(UserServer, N) ->
    for_i(N, fun(I) -> UserName = atom_to_list(node()) ++ "_" ++ integer_to_list(I),
                         spawn(stress_test, subscribe, [UserServer, UserName]) end).

blast_messages(Room) ->
    UserName = atom_to_list(node()) ++ "_1",
    for_i(1000, fun(I) -> clientserver:send_message(Room, UserName ++ "#" ++ integer_to_list(I), UserName) end).


start_blasts() ->
    spawn(fun() -> blast_messages("test_room1") end),
    spawn(fun() -> blast_messages("test_room2") end),
    spawn(fun() -> blast_messages("test_room3") end),
    spawn(fun() -> blast_messages("test_room4") end).




