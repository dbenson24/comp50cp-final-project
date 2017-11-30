-module(clientserver).
-behaviour(gen_server).


-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).



% Client Functions
start_link(UserServer, UserName) ->
    gen_server:start_link({local, clientserver}, ?MODULE, {UserServer, UserName}, []).

stop(UserName) ->
    gen_server:stop(UserName).


init({UserServer, UserName}) ->
    ok = userserver:login(UserServer, UserName),
    io:format("clientserver: logged in to ~p as ~p~n", [UserServer, UserName]),
    {ok, {UserServer, UserName, #{}, []}}.

handle_call({send_message, Message, Room}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    RoomAtom = list_to_atom(Room),
    #{RoomAtom := Node} = Rooms,
    {reply, chat:message({RoomAtom, Node}, Message), {UserServer, UserName, Rooms, MessageHandlers}};

handle_call({join_room, Room}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    RoomAtom = list_to_atom(Room),
    {ok, Node} = userserver:join_room(UserServer, RoomAtom, {clientserver, node()}),
    io:format("clientserver: joined room: ~p on ~p~n", [RoomAtom, Node]),
    {reply, ok, {UserServer, UserName, Rooms#{RoomAtom => Node}, MessageHandlers}}.
    

handle_cast({Room, message, Message}, State) ->
    % Broadcast Message to the handlers
    io:format("clientserver: Message in ~p: ~p~n", [Room, Message]),
    {noreply, State}.


terminate(normal, _State) ->
    io:format("clientserver is terminating.~n",[]),
    ok.

