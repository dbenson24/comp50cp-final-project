-module(nodemanager).
-behaviour(gen_server).


-export([start_link/1, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).



% Client Functions
start_link(MainNode) ->
    gen_server:start_link({local, nodemanager}, ?MODULE, MainNode, []).

stop() ->
    gen_server:stop(userserver).


init(MainNode) ->
    ok = userserver:register_node(MainNode, node()),
    {ok, []}.

handle_call({}, _From, Rooms) ->
    {reply, ok, Rooms}.

handle_cast({create_room, Name}, Rooms) ->
    io:format("nodemanager: starting room: ~p", [Name]),
    chat:start_link(Name),
    {noreply, Rooms};

handle_cast({create_room, Name, UserServer}, Rooms) ->
    io:format("nodemanager: starting room: ~p and subscribing: ~p~n", [Name, UserServer]),
    case lists:member(Name, Rooms) of
        true  -> gen_server:cast({Name, node()}, {subscribe, UserServer}),
                 {noreply, Rooms};
        false -> {ok, _} = chat:start_link(Name),
                 gen_server:cast({Name, node()}, {subscribe, UserServer}),
                 {noreply, [Name | Rooms]} end.


terminate(normal, _State) ->
    io:format("nodemanager is terminating.~n",[]),
    ok.


