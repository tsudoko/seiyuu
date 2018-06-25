-module(seiyuu).
-export([nyaa/1]).

nyaa(V) ->
	IDs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
	vndb:login(V, [{protocol, 1}, {client, <<"test">>}, {clientver, <<"0.1">>}]),
	%VNs = vndb:get_all(V, vn, [basic], [<<"(id = [">>, lists:join(<<",">>, lists:map(fun(X) -> integer_to_binary(X) end, IDs)), <<"])">>]),
	Chars = vndb:get_all(V, character, [basic, voiced, vns], [<<"(vn = [">>, lists:join(<<",">>, lists:map(fun(X) -> integer_to_binary(X) end, IDs)), <<"])">>]),
	Staff = vndb:get_all(V, staff, [basic, aliases], [<<"(id = [">>, lists:join(<<",">>,
		lists:map(fun(X) -> integer_to_binary(X) end,
		lists:usort(lists:map(fun(EM) -> maps:get(<<"id">>, EM) end, lists:flatten(
			lists:map(fun(M) -> maps:get(<<"voiced">>, M) end, Chars)
		))))), <<"])">>]),
	% [{staff1, [{alias1, [char1, char2...]}, {alias2...}...]}, {staff2...}...]
	StaffChars =
	lists:map(fun(S) -> {maps:get(<<"id">>, S),
		lists:map(fun([AID|_]) -> {AID,
			lists:map(fun(X) -> {CID, _} = X, CID end,
				lists:filter(fun({CID, V}) -> lists:member(AID, V) end,
					% [{cid, [{aid, aid..}...]}...]
					lists:map(fun(C) -> {maps:get(<<"id">>, C), lists:map(fun(V) -> maps:get(<<"aid">>, V) end, maps:get(<<"voiced">>, C))} end, Chars)
				)
			)} end, maps:get(<<"aliases">>, S)
		)} end, Staff
	),
	{Staff, Chars, StaffChars}.
