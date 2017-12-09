-module(msgserver).
-behaviour(gen_server).

-export([start_link/1, stop/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-export([message/3]).

% Client Functions
start_link(ServerName) ->
    gen_server:start_link({local, ServerName}, ?MODULE, ServerName, []).

stop(ServerName) ->
    gen_server:stop(ServerName).

message(Server, UserName, Message) ->
    gen_server:call(Server, {message, UserName, Message}).

% Server Functions
init(ServerName) ->
    io:format("msgserver: Starting msg room: ~p~n", [ServerName]),
    {ok, {ServerName, []}}.

handle_call({list_clients}, _From, {ServerName, Clients}) ->
    {reply, Clients, {ServerName, Clients}};

handle_call({message, UserName, Message}, _From, {ServerName, Clients}) ->
    spawn(lists, map, [fun(Client) -> gen_server:cast(Client, {message, ServerName, UserName, Message}) end, Clients]),
    {reply, ok, {ServerName, Clients}}.

handle_cast({subscribe, Client}, {ServerName, Clients}) ->
    NewClients = case lists:member(Client, Clients) of
                   true  -> Clients;
                   false -> [Client | Clients] end,
    io:format("msgserver: subscribing ~p to ~p~n", [Client, ServerName]),
    {noreply, {ServerName, NewClients}};

handle_cast({unsubscribe, Client}, {ServerName, Clients}) ->
    {noreply, {ServerName, list:delete(Client, Clients)}}.

terminate(normal, _State) ->
    io:format("msgserver is terminating.~n",[]),
    ok.

