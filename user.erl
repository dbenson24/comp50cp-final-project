-module(user).
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
    {ok, #{}}.

handle_call({login, Name}, _From, State) ->
    NewState = case maps:is_key(Name, State) of
                   true -> State;
                   false -> State#{Name => {false}} end,
    #{Name := UserState} = NewState,
    case UserState of
        {true} -> {reply, {err, "User was already logged in"}, State};
        {false} -> {reply, {ok}, State#{Name => {true}}} end; % logic here to automatically subscribe the client to updates to their chats
handle_call({logout, Name}, _From, State) ->
    {reply, {ok}, State#{Name => {false}}}.

handle_cast(_, State) ->
    {noreply, State}.


terminate(normal, _State) ->
    io:format("Server is terminating.~n",[]),
    ok.



