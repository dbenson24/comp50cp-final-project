# Comp 50CP Final Project
the-a-team-but-not-in-the-ed-sheeran-way

## Files included
- `start_server.erl`     escript for starting server with simple config
- `src/`                 erlang files
  - `clientserver.erl`   server to be run by the client
  - `userserver.erl`     server for clients to connect to, handles auth and name
  - `nodemanager.erl`    server that can spawn userservers on same node
  - `tictactoegame.erl`  example bridge between erlang client and python client
- `Makefile`             instructions for compiling erlang
- `game.py`              the implementation of the demo game
- `text_input.py`        pygame text input module
- `requirements.txt`     python dependencies

Descriptions of these files can be found in the final submission pdf document.


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
$ ./start_server.erl
```

#### Sessions 2/3: the clients
We'll spawn two clients on the local node that will communicate through
the server. If we wish to communicate to a server on a different node,
we can supply it as the first argument to start_game/2.

- `[[SHORTNAME]]` should be replaced by different unique erlang snames.
- `[[USERNAME]]`  should be replaced by different unique, typable strings.

```
$ erl -env ERL_LIBS ../erlport /src/clientserver.erl -sname [[SHORTNAME]]

Eshell:
1> tictactoegame:start_game("[[USERNAME]]").
```