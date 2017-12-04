-module(tictactoegame).

-export([start_game/2, stop_game/1, caller/0]).

caller() ->
    receive {Module, Func, Args} -> apply(Module, Func, Args), caller();
             done -> ok end.

% python:call(P, 'chat_client', 'receive_chat_default', [<<Message>>])
start_game(UserServerNode, UserName) ->
    {ok, _} = clientserver:start_link(UserServerNode, UserName),
    ok = clientserver:join_room("tictactoe"),
    {ok, P} = python:start(),
    CallerPid = spawn(?MODULE, caller, []),
    true = python:call(P, 'chat_client', 'set_erlPID', [CallerPid]), 
    true = python:call(P, 'chat_client', 'start_game_thread', []),
    {ok, _Handler} = clientserver:register_handler(fun (Message) -> io:format("process ~p receiving ~p~n", [P, Message]), python:call(P, 'chat_client', 'receive_chat_default', [list_to_binary(Message)]) end), 
    {ok, P}.

stop_game(P) ->
    true = python:call(P, 'chat_client', 'stop_game_thread', []),
    ok = clientserver:stop(),
    ok.

