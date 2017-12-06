-module(tictactoegame).

-export([start_game/1, start_game/2, stop_game/1, caller/0, test_receive/1]).

caller() ->
    receive {Module, Func, Args} -> apply(Module, Func, Args), caller();
             done -> ok end.

default_main() ->
    {_, HostName} = lists:splitwith(fun(C) -> C /= $@ end, atom_to_list(node())),
    list_to_atom("main" ++ HostName).

test_receive(UserName) ->
    {ok, _} = clientserver:start_link(default_main(), UserName),
    ok = clientserver:join_room("test_room", UserName),
    {ok, _Handler} = clientserver:register_handler(fun (Message) -> io:format("user: ~p, msg: ~p~n", [UserName, Message]) end, UserName),
    ok.

% python:call(P, 'chat_client', 'receive_chat_default', [<<Message>>])
start_game(UserName) ->
    start_game(default_main(), UserName).

start_game(UserServerNode, UserName) ->
    {ok, _} = clientserver:start_link(UserServerNode, UserName),
    ok = clientserver:join_room("tictactoe", UserName),
    {ok, P} = python:start(),
    CallerPid = spawn(?MODULE, caller, []),
    true = python:call(P, 'chat_client', 'set_erlPID', [CallerPid]), 
    true = python:call(P, 'chat_client', 'start_game_thread', [list_to_binary(UserName)]),
    {ok, _Handler} = clientserver:register_handler(
                       fun (Message) -> 
                           io:format("process ~p receiving ~p~n", [P, Message]),
                           python:call(P, 'chat_client', 'receive_chat_default', [list_to_binary(Message)]) 
                       end,
                       UserName),
    {ok, P}.

stop_game(P) ->
    true = python:call(P, 'chat_client', 'stop_game_thread', []),
    ok = clientserver:stop(),
    ok.

