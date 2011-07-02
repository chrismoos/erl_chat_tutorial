-module(chat_util).

-export([get_parameter/2,get_parameter_int/2]).
-export([get_template/2]).

-export([bin_to_hexstr/1,hexstr_to_bin/1]).

-export([generate_string/1,generate_hash/0]).

-export([unicode_clean/1]).

strip_unicode([], Cur) -> Cur;
strip_unicode([H|T], Cur) when H < 128 -> strip_unicode(T, Cur ++ [H]);
strip_unicode([_|T], Cur) -> strip_unicode(T, Cur).

unicode_clean(Str) ->
    case catch rfc4627:unicode_decode(list_to_binary(Str)) of
        {'EXIT', _Reason} -> list_to_binary(strip_unicode(Str, []));
        {'utf-8', _M} -> list_to_binary(Str);
        _ -> <<>>
    end.

get_template(Name, Vars) ->
	erlydtl:compile("templates/" ++ Name ++ ".html", list_to_atom(Name)),
	{ok, Tpl} = erlang:apply(list_to_atom(Name), render, [Vars]),
	list_to_binary(Tpl).

get_parameter_int(Name, X) ->
	case string:to_integer(get_parameter(Name, X)) of
		{error, _} -> 0;
		{Y, _} -> Y
	end.
	
get_parameter(N, [{K,V}|_]) when K == N -> V;
get_parameter(N, [_|T]) -> get_parameter(N, T);
get_parameter(_, _) -> [].

generate_hash() -> bin_to_hexstr(crypto:sha(generate_string(16))).

generate_string(Size) ->
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
    lists:flatten(lists:foldl(fun(_X,AccIn) ->
        [random:uniform(90) + 32|AccIn] end,
        [], lists:seq(1,Size))).

bin_to_hexstr(Bin) ->
  lists:flatten([io_lib:format("~2.16.0B", [X]) ||
    X <- binary_to_list(Bin)]).

hexstr_to_bin(S) ->
  try
	hexstr_to_bin(S, [])
  catch
	_:_ -> <<>>
  end.
hexstr_to_bin([], Acc) ->
  list_to_binary(lists:reverse(Acc));
hexstr_to_bin([X,Y|T], Acc) ->
  {ok, [V], []} = io_lib:fread("~16u", [X,Y]),
  hexstr_to_bin(T, [V | Acc]).