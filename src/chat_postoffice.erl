-module(chat_postoffice).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3,handle_cast/2,handle_info/2,terminate/2,code_change/3]).

-export([create_mailbox/1, delete_mailbox/1, send_mail/2, broadcast_mail/2]).


-record(state, {
mailboxes=[]
}).

start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_mailbox(ID) -> gen_server:call(?MODULE, {create_mailbox, ID}).
delete_mailbox(ID) -> gen_server:cast(?MODULE, {delete_mailbox, ID}).
send_mail(ID, Msg) -> gen_server:cast(?MODULE, {send_mail, {ID, Msg}}).
broadcast_mail(Msg, Except) -> gen_server:cast(?MODULE, {broadcast_mail, {Msg, Except}}).


get_mailbox(ID, #state{mailboxes=MBoxes}) ->
    case lists:filter(fun({Id, _}) -> Id == ID end, MBoxes) of
        [] -> {error, notfound};
        [M|_] -> {ok, M}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  gen_server exports
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(_Args) -> 
	process_flag(trap_exit, true),
	{ok, #state{}}.

handle_call({create_mailbox, ID}, _From, #state{mailboxes=MBoxes} = State) ->
    case get_mailbox(ID, State) of
        {ok, _} -> {reply, {error, already_exists}, State};
        {error, notfound} ->
            Pid = spawn_link(chat_mailbox, start, [ID]),
            NewBox = {ID, Pid},
            {reply, ok, State#state{mailboxes=[NewBox | MBoxes]}}
    end;
    	
handle_call(_Req, _From, State) -> {noreply, State}.


handle_cast({broadcast_mail, {Msg, Except}}, #state{mailboxes=MBoxes} = State) when is_list(Except) ->
    [Pid ! {mail, Msg} || {Id, Pid} <- MBoxes, lists:member(Id, Except) == false],
    {noreply, State};

handle_cast({send_mail, {ID, Msg}}, State) ->
    case get_mailbox(ID, State) of
        {ok, {_Id, Pid}} -> Pid ! {mail, Msg};
        _ -> ok
    end,
    {noreply, State};

handle_cast({delete_mailbox, ID}, #state{mailboxes=MBoxes} = State) ->
    NewBoxes = lists:filter(fun({Id, Pid}) ->
        case Id /= ID of
            false -> 
                % tell the mailbox process to quit
                Pid ! quit, false;
            _ -> true
        end
    end, MBoxes),
    {noreply, State#state{mailboxes=NewBoxes}};
    
    
handle_cast(_Request, State) -> {noreply, State}.
	
	
handle_info(_Info, State) -> 
	%error_logger:info_msg("info: ~p", [Info]),
	{noreply, State}.
terminate(_Reason, _State) -> normal.

code_change(_OldVsn, State, _Extra) -> {ok, State}.