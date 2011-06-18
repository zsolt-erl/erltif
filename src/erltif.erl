%%% File    : erltif.erl
%%% Author  : Zsolt Keszthelyi <zsolt.erl@gmail.com>
%%% Description : Erlang Terminal Interface  API module
%%% Created : 15 Jun 2011 by Zsolt Keszthelyi <zsolt.erl@gmail.com>

-module(erltif).

-compile(export_all).
-export([add_server/2, remove_server/1, add_command/6, remove_command/2]).


-spec( add_server(Name :: atom(), Port :: integer()) -> ok ).
add_server(Name, Port)->
    gen_server:call(etf_broker, {add_server, Name, Port}).

-spec( remove_server(Name :: atom())-> ok ).
remove_server(Name)->
    gen_server:call(etf_broker, {remove_server, Name}).

-spec( add_command(ServerName :: atom(), Command :: atom(), Desc :: string(), Module :: atom(), Fun :: atom(), Arity :: integer())-> ok ).
add_command(ServerName, Command, Desc, Module, Fun, Arity)->
    gen_server:call(etf_broker, {add_command, ServerName, Command, Desc, Module, Fun, Arity}).

-spec( remove_command(ServerName :: atom(), Command :: atom() )-> ok ).
remove_command(ServerName, Command)->
    gen_server:call(etf_broker, {remove_command, ServerName, Command}).

-spec( get_servers() -> list() ).
get_servers()->
    gen_server:call(etf_broker, get_servers).
