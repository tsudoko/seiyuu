-module(seiyuu).
-export([nyaa/1]).

nyaa(V) ->
	IDs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
	vndb:login(V, [{protocol, 1}, {client, <<"test">>}, {clientver, <<"0.1">>}]),
	%VNs = vndb:get_all(V, vn, [basic], [<<"(id = [">>, lists:join(<<",">>, [integer_to_binary(X) || X <- IDs]), <<"])">>]),
	Chars = vndb:get_all(V, character, [basic, voiced, vns], [<<"(vn = [">>, lists:join(<<",">>, [integer_to_binary(X) || X <- IDs]), <<"])">>]),
	Staff = vndb:get_all(V, staff, [basic, aliases], [<<"(id = [">>,
		lists:join(<<",">>, [integer_to_binary(X) || X <- lists:usort([ID || #{<<"id">> := ID} <- lists:flatten([ V || #{<<"voiced">> := V} <- Chars])])]),
		<<"])">>]),
	% [{staff1, [{alias1, [char1, char2...]}, {alias2...}...]}, {staff2...}...]
	StaffChars =
	[{S, AliasList} ||
		#{<<"id">> := S, <<"aliases">> := Aliases} <- Staff,
		AliasList <- [[{A, CharList} ||
			[A|_] <- Aliases,
			CharList <- [[C ||
				#{<<"id">> := C, <<"voiced">> := Voiced} <- Chars,
				lists:member(A, [V || #{<<"aid">> := V} <- Voiced])]],
			CharList /= []]]],
	{Staff, Chars, StaffChars}.
