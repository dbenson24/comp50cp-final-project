-module(tictactoegame).

-export([start_game/1, start_game/2, stop_game/2, caller/1, test_receive/1, send_state/4, request_start/4, send_over/3]).

caller(UserName) ->
    receive {Module, Func, Args} -> apply(Module, Func, Args), caller(UserName);
             done -> clientserver:stop(UserName) end.

default_main() ->
    {_, HostName} = lists:splitwith(fun(C) -> C /= $@ end, atom_to_list(node())),
    list_to_atom("main" ++ HostName).

test_receive(UserName) ->
    {ok, _} = clientserver:start_link(default_main(), UserName),
    ok = clientserver:join_room("test_room", UserName),
    {ok, _Handler} = clientserver:register_handler(fun (_Room, _FromUser, Message) -> io:format("user: ~p, msg: ~p~n", [UserName, Message]) end, UserName),
    ok.

send_state(Room, UserName, Target, State) ->
    Message = {tictactoe, Target, {update, State}},
    ok = clientserver:send_message(Room, Message, UserName),
    ok.

request_start(Room, UserName, Target, State) ->
    Message = {tictactoe, Target, {start, State}},
    ok = clientserver:send_message(Room, Message, UserName),
    ok.

send_over(Room, UserName, Target) ->
    Message = {tictactoe, Target, over},
    ok = clientserver:send_message(Room, Message, UserName),
    ok.

% python:call(P, 'game', 'receive_chat_default', [<<Message>>])
start_game(UserName) ->
    start_game(default_main(), UserName).

start_game(UserServerNode, UserName) ->
    {ok, _} = clientserver:start_link(UserServerNode, UserName),
    ok = clientserver:join_room("tictactoe", UserName),
    {ok, P} = python:start(),
    CallerPid = spawn(?MODULE, caller, [UserName]),
    true = python:call(P, 'game', 'set_erlPID', [CallerPid]), 
    true = python:call(P, 'game', 'start_game_thread', [list_to_binary(UserName)]),
    {ok, _Handler} = clientserver:register_handler(
        fun (_Room, FromUser, Message) ->
            io:format("message to ~p, ~p~n", [UserName, Message]),
            case Message of 
                {tictactoe, UserName, Action} ->
                    case Action of 
                        {update, State} ->
                            python:call(P, 'game', 'receive_state', [list_to_binary(FromUser), State]);
                        {start, State} ->
                            python:call(P, 'game', 'receive_start', [list_to_binary(FromUser), State]);
                        over ->
                            python:call(P, 'game', 'receive_game_over', [list_to_binary(FromUser)]) end;
                [_X | _Xs] -> python:call(P, 'game', 'receive_chat_default', [list_to_binary(Message), list_to_binary(FromUser)]);
                _ -> ok end
        end,
        UserName),
    {ok, P}.

stop_game(P, UserName) ->
    true = python:call(P, 'game', 'stop_game_thread', []),
    ok = clientserver:stop(UserName),
    ok.

