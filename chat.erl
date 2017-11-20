-module(chat).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([subscribe/3, unsubscribe/2, message/3, list/2, join_room/3]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

% Client Functions
start_link() ->
    gen_server:start_link({local, chatserver}, ?MODULE, [], []).

stop() ->
    gen_server:stop(chatserver).

subscribe(Server, Room, Name) ->
    Client = {self(), Name},
    gen_server:cast(Server, {subscribe, Room, Client}).

unsubscribe(Server, Room) ->
    gen_server:cast(Server, {unsubscribe, Room, self()}).

message(Server, Room, Message) ->
    gen_server:call(Server, {message, Room, Message}).

list(Server, Room) ->
    gen_server:call(Server, {list, Room}).

% creates a process that subcribes to and manages a chat room and then
% starts the interactive mode
join_room(ServerNode, Room, Name) ->
    Server = {chatserver, ServerNode},
    HandlerPid = spawn(fun() -> subscribe(Server, Room, Name),
                            handle_server_connection(Server, Room) end),
    watch_usr_input(HandlerPid),
    ok.
    
% all communication to chat server goes through process running this function
handle_server_connection(Server, Room) ->
    receive {message, Name, Message} ->
                io:format("~s: ~s~n", [Name, Message]),
                handle_server_connection(Server, Room);
            {send, Message} ->
                message(Server, Room, Message),
                handle_server_connection(Server, Room);
            {list} ->
                Members = maps:values(list(Server, Room)),
                io:format("Users: ~p~n", [Members]),
                handle_server_connection(Server, Room);
            {quit} -> 
                unsubscribe(Server, Room)
    end.

% handles the interactive shell
watch_usr_input(HandlerPid) ->
    S = io:get_line(">"),
    case S of
        "--list\n" -> HandlerPid ! {list},
                      watch_usr_input(HandlerPid);
        "--quit\n" -> HandlerPid ! {quit},
                      ok;
        _ -> HandlerPid ! {send, S},
             watch_usr_input(HandlerPid) end.


% Server Functions
init([]) ->
    {ok, #{}}.

handle_call({list, Room}, _From, State) ->
    #{Room := Clients} = State,
    {reply, Clients, State};

handle_call({message, Room, Message}, From, State) ->
    #{Room := Clients} = State,
    {FromPid, _} = From,
    #{FromPid := Name} = Clients,
    maps:map(fun(Pid, _) -> Pid ! {message, Name, Message} end, Clients),
    {reply, ok, State}.

handle_cast({subscribe, Room, {Pid, Name}}, State) ->
    NewState = case maps:is_key(Room, State) of
                   true  -> State;
                   false -> State#{Room => #{}} end,
    #{Room := Clients} = NewState,
    {noreply, NewState#{Room => Clients#{Pid => Name}}};

handle_cast({unsubscribe, Room, Pid}, State) ->
    #{Room := Clients} = State,
    {noreply, State#{Room => maps:remove(Pid, Clients)}}.

terminate(normal, _State) ->
    io:format("Server is terminating.~n",[]),
    ok.

