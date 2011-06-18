Overview
--------
This is a simple wrapper around the (slightly modified) telnet server from Jungerl. It makes it easy to add a telnet interface to an application.  

Installation
------------

Just download or clone and run *make*.

Usage
-----

Look at erltif.erl for the API.

### Start erltif app.

    1> application:start(erltif).
    2> erltif:add_server(server1, 7999).
    Added server [server1] on port 7999
    Servers:[{server1,<0.43.0>},
             {{<0.43.0>,port},7999},
             {<0.43.0>,server1},
             {{<0.43.0>,commands},[]},
             {{<0.43.0>,clients},[]}]
    ok
    3> erltif:add_command(server1, mem, "Erlang memory usage", erlang, memory, 0).
    Added command [mem] to server [server1]

### Connect using telnet

    zsolt@mrm:~$ telnet localhost 7999
    Trying ::1...
    Trying 127.0.0.1...
    Connected to localhost.
    Escape character is '^]'.
    >>> You are connected to telnet server [server1]
    >>> Type 'exit' to disconnect
    >>> Type 'help' for list of commands

    help
    help
    ----COMMANDS----------------------------------------------------------------------
    mem             | Erlang memory usage                      | erlang:memory/0

    mem
    mem
    [{total,7696880},
     {processes,1065200},
     {processes_used,1060152},
     {system,6631680},
     {atom,460985},
     {atom_used,433430},
     {binary,9624},
     {code,3848477},
     {ets,273784}]
