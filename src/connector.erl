-module(connector).

-compile(export_all).

connection_opened(TelnetClientPid, TelnetServerPid)->
    io:format("Connection Pid:~p~n",[TelnetClientPid]),
    gen_server:call(etf_broker, {connected, TelnetClientPid, TelnetServerPid}),
    loop(TelnetClientPid, TelnetServerPid).

loop(TelnetClientPid, TelnetServerPid)->
    case ce_telnet:get_line(TelnetClientPid) of
	{error, closed}->
	    gen_server:call(etf_broker, {client_closed, TelnetClientPid, TelnetServerPid});
	{ok, []} ->
	    loop(TelnetClientPid, TelnetServerPid);
	{ok, Line} ->
	    gen_server:call(etf_broker, {client_command, Line, TelnetClientPid, TelnetServerPid}, 30000),
	    loop(TelnetClientPid, TelnetServerPid)
    end.

