-module(userserver).
-behaviour(gen_server).



-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([login/2, logout/2, register_node/2, join_room/3]).



% Client Functions
start_link() ->
    gen_server:start_link({local, userserver}, ?MODULE, [], []).

stop() ->
    gen_server:stop(userserver).

login(UserServer, UserName) ->
    gen_server:call({userserver, UserServer}, {login, UserName}).

logout(UserServer, UserName) ->
    gen_server:call({userserver, UserServer}, {logout, UserName}).

register_node(UserServer, Node) ->
    gen_server:call({userserver, UserServer}, {register_node, Node}).

join_room(UserServer, Room, ClientServer) ->
    gen_server:call({userserver, UserServer}, {join_room, Room, ClientServer}).


% Server Functions
init([]) ->
    {ok, {#{}, #{}, [], 0}}.

handle_call({login, Name}, _From, {Users, Rooms, Nodes, I}) ->
    NewUsers = case maps:is_key(Name, Users) of
                   true -> Users;
                   false -> Users#{Name => {false}} end,
    #{Name := UserState} = NewUsers,
    case UserState of
        {true} -> {reply, {err, "User was already logged in"}, {Users, Rooms, Nodes, I}};
        {false} -> {reply, ok, {Users#{Name => {true}}, Rooms, Nodes, I}} end; % logic here to automatically subscribe the client to updates to their chats

handle_call({logout, Name}, _From, {Users, Rooms, Nodes, I}) ->
    {reply, ok, {Users#{Name => {false}}, Rooms, Nodes, I}};

handle_call({list_rooms}, _From, {Users, Rooms, Nodes, I}) ->
    {reply, Rooms, {Users, Rooms, Nodes, I}};

handle_call({register_room, Node, Name}, _From, {Users, Rooms, Nodes, I}) ->
    {Resp, NewRooms} = case maps:is_key(Name, Rooms) of
                    true  -> {{err, "Room was already registered"}, Rooms};
                    false -> {ok, Rooms#{Name => Node}} end,
    {reply, Resp, {Users, NewRooms, Nodes, I}};

handle_call({unregister_room, Name}, _From, {Users, Rooms, Nodes, I}) ->
    NewRooms = maps:remove(Name, Rooms),
    {reply, ok, {Users, NewRooms, Nodes, I}};

handle_call({join_room, RoomName, UserServer}, _From, {Users, Rooms, Nodes, I}) ->
    {Node, NewRooms} = case maps:is_key(RoomName, Rooms) of
               true  -> {maps:get(RoomName, Rooms), Rooms};
               false -> TempNode = lists:nth(I+1, Nodes),
                        {TempNode, Rooms#{RoomName => TempNode}} end,
    io:format("userserver: ~p is joining ~p on ~p~n", [UserServer, RoomName, Node]),
    gen_server:cast({nodemanager, Node}, {create_room, RoomName, UserServer}),
    {reply, {ok, Node}, {Users, NewRooms, Nodes, (I + 1) rem length(Nodes)}};

handle_call({register_node, Node}, _from, {Users, Rooms, Nodes, I}) ->
    NewNodes = case lists:member(Node, Nodes) of
                true -> Nodes;
                false -> [Node | Nodes] end,
    {reply, ok, {Users, Rooms, NewNodes, I}}.
    


handle_cast(_, {Users, Rooms, Nodes, I}) ->
    {noreply, {Users, Rooms, Nodes, I}}.



terminate(normal, _State) ->
    io:format("userserver is terminating.~n",[]),
    ok.



