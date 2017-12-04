-module(clientserver).
-behaviour(gen_server).


-export([start_link/2, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([join_room/1, send_message/2, register_handler/1, unregister_handler/1, test/0]).


% Client Functions
start_link(UserServer, UserName) ->
    gen_server:start_link({local, clientserver}, ?MODULE, {UserServer, UserName}, []).

stop() ->
    gen_server:stop(clientserver).

join_room(Room) ->
    gen_server:call({clientserver, node()}, {join_room, list_to_atom(Room)}).

send_message([R | Oom], [M | Essage]) ->
    Room = list_to_atom([R | Oom]),
    Message = [M | Essage],
    gen_server:call({clientserver, node()}, {send_message, Room, Message});

send_message(<<R,Oom/binary>>, <<M,Essage/binary>>) ->
    Room = list_to_atom(binary_to_list(<<R,Oom/binary>>)),
    Message = binary_to_list(<<M,Essage/binary>>),
    gen_server:call({clientserver, node()}, {send_message, Room, Message}).

test() -> io:format("clientserver test!~n"), fuckerlport.

register_handler(F) ->
    ok = gen_server:call({clientserver, node()}, {register_handler, F}),
    {ok, F}.

unregister_handler(F) ->
    ok = gen_server:call({clientserver, node()}, {unregister_handler, F}),
    ok.



init({UserServer, UserName}) ->
    ok = userserver:login(UserServer, UserName),
    io:format("clientserver: logged in to ~p as ~p~n", [UserServer, UserName]),
    {ok, {UserServer, UserName, #{}, []}}.

handle_call({send_message, RoomAtom, Message}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    #{RoomAtom := Node} = Rooms,
    {reply, chat:message({RoomAtom, Node}, Message), {UserServer, UserName, Rooms, MessageHandlers}};

handle_call({join_room, RoomAtom}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    {ok, Node} = userserver:join_room(UserServer, RoomAtom, {clientserver, node()}),
    io:format("clientserver: joined room: ~p on ~p~n", [RoomAtom, Node]),
    {reply, ok, {UserServer, UserName, Rooms#{RoomAtom => Node}, MessageHandlers}};
    

handle_call({register_handler, Handler}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    {reply, ok, {UserServer, UserName, Rooms, [Handler | MessageHandlers]}};

handle_call({unregister_handler, Handler}, _From, {UserServer, UserName, Rooms, MessageHandlers}) ->
    {reply, ok, {UserServer, UserName, Rooms, lists:delete(Handler, MessageHandlers)}}.

handle_cast({message, Room, Message}, {UserServer, UserName, Rooms, MessageHandlers}) ->
    % Broadcast Message to the handlers
    io:format("clientserver: Message in ~p: ~p~n", [Room, Message]),
    lists:map(fun (F) -> F(Message) end, MessageHandlers),
    {noreply, {UserServer, UserName, Rooms, MessageHandlers}}.


terminate(normal, {UserServer, UserName, _, _}) ->
    io:format("clientserver is terminating.~n",[]),
    ok = userserver:logout(UserServer, UserName),
    ok.

