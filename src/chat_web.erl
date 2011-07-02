-module(chat_web).

-export([start_link/0]).

-export([loop/1]).

-export([wait_msg_id/1,wait/1,timeout_wait/1]).

-define(COMET_TIMEOUT, 30000).

start_link() -> mochiweb_http:start([{port, 8000}, {loop, {?MODULE, loop}}]).

% Request helpers
get_session(Req) -> chat_util:get_parameter("chat_sess", Req:parse_cookie()).

% Response Helpers
html_ok(Req, Data) -> Req:ok({"text/html;charset=UTF-8", Data}).
bad_session(Req) -> json_respond(json_client_error(<<"bad_session">>), Req).

% JSON Helpers
json_respond(Msg, Req) -> Req:ok({"text/json", [], Msg}).
json_client_ok(Msg) -> lists:flatten(rfc4627:encode({obj, [{"status", <<"ok">>}, {response, Msg}]})).
json_client_error(Msg) -> lists:flatten(rfc4627:encode({obj, [{"status", <<"error">>}, {response, Msg}]})).

format_data(user_left_room, {Nick, Reason}) -> {obj, [{"nick", Nick}, {"reason", Reason}]};
format_data(user_joined_room, Nick) -> Nick;
format_data(system_msg, Msg) -> Msg;
format_data(X, {Nick, Msg}) when (X == chat_msg) or (X == sent_chat_msg) -> {obj, [{"nick", Nick}, {"msg", Msg}]};
format_data(_, _) -> [].

format_message({Id, {Type, Data}}) -> {obj, [{"id", Id}, {"t", list_to_binary(atom_to_list(Type))}, {"d", format_data(Type, Data)}]};
format_message(_) -> [].

% Request loops
wait_msg_id(Req) ->
    receive
        {cur_msg_id, MsgID} -> json_respond(json_client_ok(MsgID), Req);
        _ -> bad_session(Req)
    end.

wait(Req) ->
    receive
        timeout -> json_respond(json_client_ok(<<"reconnect">>),Req);
        Items when is_list(Items) -> 
            Msgs = lists:flatten(lists:map(fun(X) -> format_message(X) end, Items)),
            json_respond(json_client_ok(Msgs), Req);
        _ -> bad_session(Req)
    end,
    chat_room:wait_finish(get_session(Req), self()).

timeout_wait(Pid) -> Pid ! timeout.    
    
% Request handlers

handle_request(Req, "/chat/send_msg/") ->
    chat_room:chat_message(get_session(Req), chat_util:get_parameter("msg", Req:parse_post())),
    json_respond(json_client_ok(<<"">>), Req);

handle_request(Req, "/chat/leave/") -> 
    chat_room:leave(get_session(Req), "normal"),
    json_respond(json_client_ok(<<>>), Req);

handle_request(Req, "/chat/wait/") ->
    MsgID = chat_util:get_parameter_int("msg_id", Req:parse_qs()),
    chat_room:wait(get_session(Req), MsgID, self()),
    timer:apply_after(?COMET_TIMEOUT, ?MODULE, timeout_wait, [self()]),
    proc_lib:hibernate(?MODULE, wait, [Req]);

handle_request(Req, "/chat/start/") ->
    chat_room:get_msg_id(get_session(Req), self()),
    proc_lib:hibernate(?MODULE, wait_msg_id, [Req]);
    
handle_request(Req, "/chat/online/") ->
    case chat_room:get_users(get_session(Req)) of
        {ok, Users} -> json_respond(json_client_ok(Users), Req);
        _ -> bad_session(Req)
    end;
    
handle_request(Req, "/chat/") -> html_ok(Req, chat_util:get_template("chat", []));

handle_request(Req, "/login/") ->
    Post = Req:parse_post(),
	case chat_room:join(chat_util:get_parameter("nick", Post), Req:get(peer)) of
	    {ok, SessID} -> 
	        SessCookie = mochiweb_cookies:cookie("chat_sess", SessID, [{path, "/"}]),
	        Req:respond({302, [SessCookie, {"Location", "/chat/"}], <<>>});
		{error, not_available} -> html_ok(Req, chat_util:get_template("index", [{error, "The nickname is not available."}]));
	    _ -> html_ok(Req, chat_util:get_template("index", [{error, "The nickname must be alphanumeric and not blank."}]))
	end;
    
handle_request(Req, "/") -> html_ok(Req, chat_util:get_template("index", []));

handle_request(Req, Path) ->
	Req:serve_file(string:sub_string(Path, 2), "docroot", []).
	
loop(Req) ->
	catch case Req:get(version) of
	    Version when Version >= {1, 1} -> 
	        Path = Req:get(path),
	        handle_request(Req, Path);
	    _ -> ok
	end.