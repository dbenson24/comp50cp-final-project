# Comp 50CP Final Project
the-a-team-but-not-in-the-ed-sheeran-way

## Files included
- `start_server.erl`      escript to a server on main@[LOCAL_NODE]
- `src/`
  - `nodemanager.erl`     <<<TODO>>>
  - `clientserver.erl`    <<<TODO>>>
  - `userserver.erl`      <<<TODO>>>
  - `tictactoegame.erl`   <<<TODO>>>
  - `chat.erl`            <<<TODO>>>
- `Makefile`              compiles the erlang files
- `game.py`               contains the demo game engine built with pygame
- `text_input.py`         contains text manipulation (attributed in file header)


## How to use

### Dependencies
- erlang    install as directed during the course
- erlport   install as directed during the course
- python2   installed by default on most machines
- pygame    install with requirements.txt `pip install -r requirements.txt`

### How to build
Run `make` to compile the erlang files.

### How to run
The server can be run with the demo by using three terminal sessions as follows:

#### Session 1: the server
The server starts running on the current node. It uses the sname "main".
```
./start_server.erl
```

#### Sessions 2/3: the clients
We'll spawn two clients on the local node that will communicate through
the server. If we wish to communicate to a server on a different node,
we can supply it as the first argument to start_game/2.

- [[SHORTNAME]] should be replaced by different unique erlang snames.
- [[USERNAME]]  should be replaced by different unique, typable strings.

```
erl -env ERL_LIBS ../erlport /src/clientserver.erl -sname [[SHORTNAME]]

Eshell:
1> tictactoegame:start_game("[[USERNAME]]").
```