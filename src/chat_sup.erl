-module(chat_sup).
-behaviour(supervisor).

-export([start_link/1, init/1]).


start_link(Args) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, Args).
	
init([]) ->
    crypto:start(),
	{ok, {{one_for_one, 3, 10},
	[
	{chat_web, 
		{chat_web, start_link, []}, permanent, 5000, worker, [chat_web]},
    {chat_postoffice, 
    	{chat_postoffice, start_link, []}, permanent, 5000, worker, [chat_postoffice]},
    {chat_room, 
    	{chat_room, start_link, []}, permanent, 5000, worker, [chat_room]}
	]}}.