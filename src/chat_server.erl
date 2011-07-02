-module(chat_server).

-behaviour(application).
-export([start/2, stop/1, startapp/0]).

startapp() -> application:start(chat_server).

start(_Type, StartArgs) -> chat_sup:start_link(StartArgs).
	
stop(_State) -> ok.
