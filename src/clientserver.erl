-module(clientserver).
-behaviour(gen_server).


-export([start_link/2, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([join_room/2, send_message/3, register_handler/2, unregister_handler/2]).


% Client Functions
start_link(UserServer, UserName) ->
    ServerAtom = username_to_serveratom(UserName),
    {ok, _} = gen_server:start_link({local, ServerAtom}, ?MODULE, {UserServer, UserName}, []),
    {ok, ServerAtom}.

stop(UserName) ->
    ServerAtom = username_to_serveratom(UserName),
    gen_server:stop(ServerAtom).

join_room(Room, UserName) ->
    ServerAtom = username_to_serveratom(UserName),
    gen_server:call({ServerAtom, node()}, {join_room, list_to_atom(Room)}).

send_message([R | Oom], Message, UserName) ->
    ServerAtom = username_to_serveratom(UserName),
    Room = list_to_atom([R | Oom]),
    gen_server:call({ServerAtom, node()}, {send_message, Room, Message});

send_message(<<R,Oom/binary>>, <<M,Essage/binary>>, UserName) ->
    ServerAtom = username_to_serveratom(UserName),
    Room = list_to_atom(binary_to_list(<<R,Oom/binary>>)),
    Message = binary_to_list(<<M,Essage/binary>>),
    gen_server:call({ServerAtom, node()}, {send_message, Room, Message}).

register_handler(F, UserName) ->
    ServerAtom = username_to_serveratom(UserName),
    ok = gen_server:call({ServerAtom, node()}, {register_handler, F}),
    {ok, F}.

unregister_handler(F, UserName) ->
    ServerAtom = username_to_serveratom(UserName),
    ok = gen_server:call({ServerAtom, node()}, {unregister_handler, F}),
    ok.

username_to_serveratom(UserName) ->
    list_to_atom(UserName ++ "_clientserver").


init({UserServer, UserName}) ->
    ok = userserver:login(UserServer, UserName),
    io:format("clientserver: logged in to ~p as ~p~n", [UserServer, UserName]),
    {ok, {UserServer, UserName, #{}, []}}.

handle_call({send_message, RoomAtom, Message}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    #{RoomAtom := Node} = Rooms,
    {reply, msgserver:message({RoomAtom, Node}, UserName, Message), {UserServer, UserName, Rooms, MessageHandlers}};

handle_call({join_room, RoomAtom}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    ServerAtom = username_to_serveratom(UserName),
    {ok, Node} = userserver:join_room(UserServer, RoomAtom, {ServerAtom, node()}),
    io:format("clientserver: joined room: ~p on ~p~n", [RoomAtom, Node]),
    {reply, ok, {UserServer, UserName, Rooms#{RoomAtom => Node}, MessageHandlers}};
    

handle_call({register_handler, Handler}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    {reply, ok, {UserServer, UserName, Rooms, [Handler | MessageHandlers]}};

handle_call({unregister_handler, Handler}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    {reply, ok, {UserServer, UserName, Rooms, lists:delete(Handler, MessageHandlers)}}.

handle_cast({message, Room, FromUser, Message}, {UserServer, UserName, Rooms, MessageHandlers}) ->
    % Broadcast Message to the handlers
    lists:map(fun (F) -> F(Room, FromUser, Message) end, MessageHandlers),
    {noreply, {UserServer, UserName, Rooms, MessageHandlers}}.


terminate(normal, {UserServer, UserName, _, _}) ->
    io:format("clientserver: is terminating.~n",[]),
    ok = userserver:logout(UserServer, UserName),
    ok.

