-module(chat_mailbox).

-export([start/1,loop/1]).

-record(message, {
id,data
}).

-record(state, {
id,cur_id=0,listeners=[],messages=[]
}).

notify_listeners(#state{listeners=Listeners,messages=Msgs,cur_id=CurID} = State) ->
    NewListeners = lists:filter(fun({MsgID, Pid}) ->
        case MsgID >= CurID of
            true -> true;
            _ ->
                % Select messages that are greater than or equal to the requested ID
                case lists:filter(fun(#message{id=ID}) -> ID >= MsgID end, Msgs) of
                    [] -> true; % no messages were found for this listener, keep it in the list
                    M -> Pid ! lists:map(fun(#message{data=Data,id=MID}) -> {MID, Data} end, M), false % remove it
                end
        end
    end, Listeners),
    State#state{listeners=NewListeners}.


loop(#state{cur_id=CurID,messages=Msgs,listeners=Listeners} = State) ->
    receive
        {mail, {add_listener, {_MsgID, _Pid} = Listener}} ->
            NewState = notify_listeners(State#state{listeners=[Listener | Listeners]}),
            proc_lib:hibernate(?MODULE, loop, [NewState]);
        {mail, {remove_listener, Pid}} ->
            NewListeners = lists:filter(fun({_Id, P}) -> P /= Pid end, Listeners),
            proc_lib:hibernate(?MODULE, loop, [State#state{listeners=NewListeners}]);
        {mail, {get_msg_id, Pid}} ->
            Pid ! {cur_msg_id, CurID},
            proc_lib:hibernate(?MODULE, loop, [State]);
        {mail, {msg, Data}} ->
            Msg = #message{id=CurID,data=Data},
            NewState = notify_listeners(State#state{messages=[Msg | Msgs],cur_id=CurID+1}),
            proc_lib:hibernate(?MODULE, loop, [NewState]);
        quit -> ok;
        _ -> proc_lib:hibernate(?MODULE, loop, [State])
    end.
    

start(ID) ->
    proc_lib:hibernate(?MODULE, loop, [#state{id=ID}]).