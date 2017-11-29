-module(userserver).
-behaviour(gen_server).



-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).



% Client Functions
start_link() ->
    gen_server:start_link({local, userserver}, ?MODULE, [], []).

stop() ->
    gen_server:stop(userserver).


% Server Functions
init([]) ->
    {ok, {#{}, #{}}}.

handle_call({login, Name}, _From, {Users, Rooms}) ->
    NewUsers = case maps:is_key(Name, Users) of
                   true -> Users;
                   false -> Users#{Name => {false}} end,
    #{Name := UserState} = NewUsers,
    case UserState of
        {true} -> {reply, {err, "User was already logged in"}, {Users, Rooms}};
        {false} -> {reply, {ok}, {Users#{Name => {true}}, Rooms}} end; % logic here to automatically subscribe the client to updates to their chats
handle_call({logout, Name}, _From, {Users, Rooms}) ->
    {reply, {ok}, {Users#{Name => {false}}, Rooms}};
handle_call({list_rooms}, _From, {Users, Rooms}) ->
    {reply, Rooms, {Users, Rooms}};
handle_call({register_room, Node, Name}, _From, {Users, Rooms}) ->
    {Resp, NewRooms} = case maps:is_key(Name, Rooms) of
                    true -> {{err, "Room was already registered"}, Rooms };
                    false -> {{ok}, Rooms#{Name => Node}} end,
    {reply, Resp, {Users, NewRooms}};
handle_call({unregister_room, Name}, _From, {Users, Rooms}) ->
    NewRooms = maps:remove(Name, Rooms),
    {reply, {ok}, {Users, NewRooms}}.

handle_cast(_, {Users, Rooms}) ->
    {noreply, {Users, Rooms}}.


terminate(normal, _State) ->
    io:format("Server is terminating.~n",[]),
    ok.



