-module(nodemanager).
-behaviour(gen_server).


-export([start_link/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).



% Client Functions
start_link() ->
    gen_server:start_link({local, nodemanager}, ?MODULE, [], []).

stop() ->
    gen_server:stop(userserver).


init([]) ->
    {ok, []}.

handle_call({}, _From, Rooms) ->
    {reply, {ok}, Rooms}.

handle_cast({create_room, Name}, Rooms) ->
   chat:start_link(Name),
   {noreply, Rooms};
handle_cast({create_room, Name, UserServer}, Rooms) ->
    case list:member(Name, Rooms) of
        true  -> {noreply, [Name | Rooms]};
        false -> gen_server:cast({node(), Name}, {subscribe, UserServer}),
                 {noreply, Rooms} end.


terminate(normal, _State) ->
    io:format("Server is terminating.~n",[]),
    ok.


