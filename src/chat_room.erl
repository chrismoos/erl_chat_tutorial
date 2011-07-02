-module(chat_room).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([join/2,leave/2,wait/3,wait_finish/2,chat_message/2,get_users/1,get_msg_id/2,find_idle_clients/0]).

% Time before a client is considered gone
-define(MAX_IDLE_TIME, 120).
-define(CHECK_IDLE_TIME, 60).

-record(client_state, {
id, nick, host,last_action
}).

-record(state, {
clients=[]
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

join(Nick, Host) -> gen_server:call(?MODULE, {join, {Nick, Host}}, infinity).
leave(Sess, Reason) -> gen_server:cast(?MODULE, {leave, {Sess, Reason}}).
chat_message(Sess, Msg) -> gen_server:cast(?MODULE, {chat_message, {Sess, Msg}}).
wait(Sess, MsgID, Pid) -> gen_server:cast(?MODULE, {wait, {Sess, MsgID, Pid}}).
wait_finish(Sess, Pid) -> gen_server:cast(?MODULE, {wait_finish, {Sess, Pid}}).
get_users(Sess) -> gen_server:call(?MODULE, {get_users, Sess}, infinity).
get_msg_id(Sess, Pid) -> gen_server:cast(?MODULE, {get_msg_id, {Sess, Pid}}).
find_idle_clients() -> gen_server:cast(?MODULE, find_idle_clients).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  chat_room helpers
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
validate_nick([], _) -> {error, bad_format};
validate_nick(Nick, #state{clients=Clients}) ->
    Shortened = list_to_binary(lists:sublist(string:strip(Nick), 16)),
    case {re:run(binary_to_list(Shortened), "^([A-Za-z0-9]+)$"), lists:filter(fun(#client_state{nick=N}) -> N == Shortened end, Clients)} of
        {{match, _}, []} -> {ok, Shortened};
        {nomatch, _} -> {error, bad_format};
        _ -> {error, not_available}
    end.
    
get_unique_session(#state{clients=Clients} = State) ->
	X = chat_util:generate_hash(),
	case lists:filter(fun(#client_state{id=ID}) -> ID == X end, Clients) of % Make sure the session ID is unique
		[] -> X;
		_ -> get_unique_session(State)
	end.
	
get_session(Session, #state{clients=Clients}) ->
	case lists:filter(fun(#client_state{id=ID}) -> ID == Session end, Clients) of
		[] -> {error, not_found};
		[X|_] -> {ok, X}
	end.

update_client(Client, #state{clients=Clients} = State) -> 
    NewClient = Client#client_state{last_action=now()},
    Others = lists:filter(fun(#client_state{id=ID}) -> ID /= Client#client_state.id end, Clients),
    State#state{clients=[NewClient | Others]}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> 
	process_flag(trap_exit, true),
	timer:apply_after(?CHECK_IDLE_TIME, ?MODULE, find_idle_clients, []),
	{ok, #state{}}.

handle_call({join, {Nick, Host}}, _From, #state{clients=Clients} = State) when is_list(Nick) ->
    case validate_nick(Nick, State) of
        {error, Reason} -> {reply, {error, Reason}, State};
        {ok, ValidNick} ->
            Session = get_unique_session(State),
            case chat_postoffice:create_mailbox(Session) of
                ok -> 
                    chat_postoffice:broadcast_mail({msg, {user_joined_room, ValidNick}}, [Session]),
                    Client = #client_state{id=Session,nick=ValidNick,host=Host,last_action=now()},
                    {reply, {ok, Session}, State#state{clients=[Client | Clients]}};
                {error, _} -> {reply, {error, not_available}, State}
            end
    end;

handle_call({get_users, Sess}, _From, State) ->
    case get_session(Sess, State) of
        {error, not_found} -> {reply, {error, not_found}, State};
        {ok, C} -> 
            NewState = update_client(C, State),
            {reply, {ok, lists:map(fun(#client_state{nick=Nick}) -> Nick end, State#state.clients)}, NewState}
    end;

handle_call(_Req, _From, State) -> {noreply, State}.

handle_cast({get_msg_id, {Sess,Pid}}, State) ->
    case get_session(Sess, State) of
        {error, not_found} -> 
            Pid ! {error, bad_session},
            {noreply, State};
        {ok, C} -> 
            NewState = update_client(C, State),
            chat_postoffice:send_mail(Sess, {get_msg_id, Pid}), {noreply, NewState}
    end;

handle_cast(find_idle_clients, #state{clients=Clients} = State) ->
	lists:foreach(fun(Client) ->
		LastAction = calendar:now_to_datetime(Client#client_state.last_action),
		Now = calendar:now_to_datetime(now()),
		IdleSecs =  calendar:datetime_to_gregorian_seconds(Now) - calendar:datetime_to_gregorian_seconds(LastAction),
		case IdleSecs > ?MAX_IDLE_TIME of
			true -> 
			    error_logger:info_msg("User timed out: ~p, secs: ~p", [Client#client_state.nick, IdleSecs]),
				timer:apply_after(0, ?MODULE, leave, [Client#client_state.id, "timeout"]);
			_ -> noop
		end
	end, Clients),
	timer:apply_after(?CHECK_IDLE_TIME, ?MODULE, find_idle_clients, []),
	{noreply, State};
    
handle_cast({wait, {Sess, MsgID, Pid}}, State) when is_integer(MsgID) ->
    case get_session(Sess, State) of
        {error, not_found} -> Pid ! {error, bad_session}, {noreply, State};
        {ok, C} ->
            NewState = update_client(C, State), 
            chat_postoffice:send_mail(Sess, {add_listener, {MsgID, Pid}}), {noreply, NewState}
    end;
    
handle_cast({wait_finish, {Sess, Pid}}, State) ->
    case get_session(Sess, State) of
        {error, not_found} -> {noreply, State};
        {ok, _} -> chat_postoffice:send_mail(Sess, {remove_listener, Pid}), {noreply, State}
    end;
    
handle_cast({chat_message, {Sess, Msg}}, State) when is_list(Msg) ->
    case get_session(Sess, State) of
        {error, not_found} -> {noreply, State};
        {ok, #client_state{nick=Nick,id=ID} = C} ->
            CleanMsg = chat_util:unicode_clean(lists:sublist(Msg, 256)),
            chat_postoffice:broadcast_mail({msg, {chat_msg, {Nick, CleanMsg}}}, [ID]),
            chat_postoffice:send_mail(ID, {msg, {sent_chat_msg, {Nick, CleanMsg}}}),
            NewState = update_client(C, State),
            {noreply, NewState}
    end;

handle_cast({leave, {Sess, Reason}}, #state{clients=Clients} = State) when is_list(Reason) ->
    case get_session(Sess, State) of
        {error, not_found} -> {noreply, State};
        {ok, Client} ->
            chat_postoffice:delete_mailbox(Client#client_state.id),
            CleanReason =  chat_util:unicode_clean(lists:sublist(Reason, 32)),
            chat_postoffice:broadcast_mail({msg, {user_left_room, {Client#client_state.nick, CleanReason}}}, [Client#client_state.id]),
            OtherClients = lists:filter(fun(#client_state{id=ID}) -> ID /= Client#client_state.id end, Clients),
            {noreply, State#state{clients=OtherClients}}
    end;    

handle_cast(_Request, State) -> {noreply, State}.
	
	
handle_info(Info, State) -> 
	error_logger:info_msg("info: ~p", [Info]),
	{noreply, State}.
terminate(_Reason, _State) -> normal.

code_change(_OldVsn, State, _Extra) -> {ok, State}.