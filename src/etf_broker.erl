%%%-------------------------------------------------------------------
%%% File    : etf_broker.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : handles registering telnet servers and connects them 
%%%           to your app.
%%%
%%% Created : 14 Jun 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%%-------------------------------------------------------------------
-module(etf_broker).

-behaviour(gen_server).

-define(MAX_CONNECTIONS, 5).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).


%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([]) ->
    {ok, dict:new()}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({add_server, Name, Port}, _From, Servers)->
    TelnetServerPid=ce_telnet:start(connector, connection_opened, Port, ?MAX_CONNECTIONS),
    %% unlink it so that etf_broker won't die when a telnet server exits
    unlink(TelnetServerPid),
    NewServer=dict:from_list( [{Name, TelnetServerPid},
			       {TelnetServerPid, Name},
			       {{TelnetServerPid, port}, Port}, 
			       {{TelnetServerPid, clients}, []}, 
			       {{TelnetServerPid, commands}, []}] ),

    
    %% adding new server to existing servers (if one exists with this name already it will keep it)
    NewServers=dict:merge(fun(_K, V1, _V2)->V1 end, Servers, NewServer),

    io:format("Added server [~p] on port ~p~n", [Name, Port]),
    io:format("Servers:~p~n", [dict:to_list(NewServers)]),

    {reply, ok, NewServers};

handle_call({remove_server, Name}, _From, Servers)->
    TelnetServerPid=dict:fetch(Name, Servers),
    stop_server_and_clients(TelnetServerPid, Servers),

    %% delete all records belonging to this server
    NewServers=remove_server(TelnetServerPid, Servers),
    
    io:format("Removed server [~p]~n", [Name]),
    io:format("Servers:~p~n", [dict:to_list(NewServers)]),

    {reply, ok, NewServers};

handle_call({connected, TelnetClientPid, TelnetServerPid}, _From, Servers)->
    Name=dict:fetch(TelnetServerPid, Servers),

    NewServers=dict:append({TelnetServerPid, clients}, TelnetClientPid, Servers),
    ce_telnet:send(TelnetClientPid, ">>> You are connected to telnet server ["++atom_to_list(Name)++"]\n"),
    ce_telnet:send(TelnetClientPid, ">>> Type 'exit' to disconnect\n>>> Type 'help' for list of commands\n\n"),

    io:format("Connections to ~p: ~p~n", [Name, dict:fetch({TelnetServerPid, clients}, NewServers)]),
    {reply, ok, NewServers};
    
handle_call({client_command, Line, TelnetClientPid, TelnetServerPid}, _From, Servers) ->
    [CommandStr|Args]=string:tokens(Line, " "),
    Command=list_to_atom(CommandStr),
    NewServers=
	case Command of 
	    exit ->
		exit(TelnetClientPid, kill),
		NS=dict:update({TelnetServerPid, clients}, 
			       fun(CurrentClients)-> CurrentClients--[TelnetClientPid] end,
			       Servers),
		Name=dict:fetch(TelnetServerPid, Servers),
		io:format("Connections to ~p: ~p~n", [Name, dict:fetch({TelnetServerPid, clients}, NS)]),
		NS;
	    help ->
		show_help(TelnetClientPid, TelnetServerPid, Servers),
		Servers;
	    _Other ->
		process_command(Command, Args, TelnetClientPid, TelnetServerPid, Servers),
		Servers	    
	end,
    {reply, ok, NewServers};

handle_call({client_closed, TelnetClientPid, TelnetServerPid}, _From, Servers) ->
    NewServers=dict:update({TelnetServerPid, clients}, 
			   fun(CurrentClients)-> CurrentClients--[TelnetClientPid] end,
			   Servers),
    Name=dict:fetch(TelnetServerPid, Servers),
    io:format("Connections to ~p: ~p~n", [Name, dict:fetch({TelnetServerPid, clients}, NewServers)]),
    {reply, ok, NewServers};

handle_call({add_command, ServerName, Command, Desc, Module, Fun, Arity}, _From, Servers)->
    TelnetServerPid=dict:fetch(ServerName, Servers),
    NewServers=dict:append({TelnetServerPid, commands}, {Command, Desc, Module, Fun, Arity}, Servers),
    io:format("Added command [~p] to server [~p]~n", [Command, ServerName]),
    {reply, ok, NewServers};

handle_call({remove_command, ServerName, Command}, _From, Servers)->
    TelnetServerPid=dict:fetch(ServerName, Servers),
    NewServers=dict:update({TelnetServerPid, commands}, 
			   fun(CurrentCommands)-> lists:keydelete(Command, 1, CurrentCommands) end,
			   Servers),
    io:format("Removed command [~p] from server [~p]~n", [Command, ServerName]),
    {reply, ok, NewServers};

handle_call(get_servers, _From, Servers) ->
    Reply = dict:to_list(Servers),
    {reply, Reply, Servers};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

%% handle_info({'DOWN', _MonitorRef, _Type, TelnetServerPid, _Info}, Servers)->
%%     io:format("Got DOWN msg from: ~p~n", [TelnetServerPid]),
%%     case dict:find(TelnetServerPid, Servers) of
%% 	error ->
%% 	    %% msg did not come from a server process (or it was already removed from the Servers dict.)
%% 	    {noreply, Servers};
%% 	{ok, _Name} ->
%% 	    %% remove it from Servers
%% 	    NewServers=remove_server(TelnetServerPid, Servers),
%% 	    {noreply, NewServers}
%%     end;
	    
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------


process_command(Command, Args, TelnetClientPid, TelnetServerPid, Servers) ->
    ServerCommands=dict:fetch({TelnetServerPid, commands}, Servers),
    CommandStr=atom_to_list(Command),
    case lists:keyfind(Command, 1, ServerCommands) of
	false ->
	    ce_telnet:send(TelnetClientPid, ">>> Unknown command: "++CommandStr++"\n");
	{Command, _Desc, Module, Fun, Arity}->
	    case length(Args) of
		Arity -> 
		    Result=apply(Module, Fun, Args),
		    ResultStr=io_lib:format("~p", [Result]),
		    ResultPretty=prettypr:format( prettypr:text_par(ResultStr), 50 )++"\n\n",
		    ce_telnet:send(TelnetClientPid, ResultPretty);
		_Else ->
		    ce_telnet:send(TelnetClientPid, lists:concat([
								  ">>> Number of arguments needed for command [",
								 CommandStr, "]: ", Arity, "\n"
								 ]))
	    end
    end.


show_help(TelnetClientPid, TelnetServerPid, Servers) ->
    Commands=dict:fetch({TelnetServerPid, commands}, Servers),
    CommandInfos=
	lists:map(fun({Command, Desc, Module, Fun, Arity})->
			  lists:concat([
					string:left(atom_to_list(Command), 15)," | ",
					string:left(Desc, 40)," | ",
					Module, ":", Fun, "/", Arity, "\n"
				       ])
		  end,
		  Commands),
    Info=lists:concat(CommandInfos),
    ce_telnet:send(TelnetClientPid, "----COMMANDS"++string:chars($-, 70, "\n")),
    ce_telnet:send(TelnetClientPid, Info).

remove_server(TelnetServerPid, Servers)->
    Name=dict:fetch(TelnetServerPid, Servers),
    dict:filter(fun(Key, _Value)->
				   case Key of
				       Name                 -> false;
				       TelnetServerPid      -> false;
				       {TelnetServerPid, _} -> false;
				       _Else                -> true
				   end
			   end,
		Servers).

stop_server_and_clients(TelnetServerPid, Servers)->
    Clients=dict:fetch({TelnetServerPid, clients}, Servers),
    [exit(Pid, stop) || Pid <- Clients],
    ce_telnet:stop(TelnetServerPid).
							 
