-module(seiyuu).
-export([nyaa/1, start/0, loop/2, query/2]).
-export([q/3]).

nyaa(V) ->
	IDs = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
	vndb:login(V, [{protocol, 1}, {client, <<"test">>}, {clientver, <<"0.1">>}]),
	query(V, IDs).

start() ->
	Auth = [{protocol, 1}, {client, <<"test">>}, {clientver, <<"0.1">>}],
	V = vndb:connect(),
	vndb:login(V, Auth),
	PID = spawn(seiyuu, loop, [V, Auth]),
	register(seiyuu_vndb, PID),
	PID.

loop(V, Auth) ->
	receive {query, PID, IDs} -> PID ! seiyuu:query(V, IDs) end,
	loop(V, Auth).

bool(0) -> false;
bool(N) when N+1 == N+1 -> true.

query(V, IDs) ->
	VNs = vndb:get_all(V, vn, [basic], [<<"(id = [">>, lists:join(<<",">>, [integer_to_binary(X) || X <- IDs]), <<"])">>]),
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
	{[], Staff, Chars, StaffChars}.


char_name(Chars, ID, Orig) when Orig == false ->
	[N] = [Name || #{<<"id">> := CID, <<"name">> := Name} <- Chars, CID == ID],
	N;
char_name(Chars, ID, Orig) when Orig == true ->
	[N] = [Name || #{<<"id">> := CID, <<"name">> := Fallback} = C <- Chars, Name = map:get(<<"original">>, C, Fallback), CID == ID],
	N.
alias_name(Staff, ID, Orig) when Orig == false ->
	%io:format("~p~n", [Staff]),
	[N] = [Name || #{<<"aliases">> := Aliases} <- Staff, [AID, Name, _] <- Aliases, AID == ID],
	N;
% TODO: the 3rd element might be null
alias_name(Staff, ID, Orig) when Orig == true ->
	[N] = [Name || #{<<"aliases">> := Aliases} <- Staff, [AID, _, Name] <- Aliases, AID == ID],
	N.
staff_name(Staff, ID, Orig) when Orig == false ->
	[N] = [Name || #{<<"id">> := SID, <<"name">> := Name} <- Staff, SID == ID],
	N;
staff_name(Staff, ID, Orig) when Orig == true ->
	[N] = [Name || #{<<"id">> := SID, <<"name">> := Fallback} = S <- Staff, Name = map:get(<<"original">>, S, Fallback), SID == ID],
	N.
vn_name(VNs, ID, Orig) when Orig == false ->
	[N] = [Name || #{<<"id">> := VID, <<"title">> := Name} <- VNs, VID == ID],
	N;
vn_name(VNs, ID, Orig) when Orig == true ->
	[N] = [Name || #{<<"id">> := VID, <<"title">> := Fallback} = V <- VNs, Name = map:get(<<"original">>, V, Fallback), VID == ID],
	N.

format_table({_, Staff, Chars, Table}, Orig) ->
	[{staff_name(Staff, S, Orig),
		[{alias_name(Staff, A, Orig),
			[char_name(Chars, C, Orig) || C <- Cl]}
		|| {A, Cl} <- Al]}
	|| {S, Al} <- Table].

table_html(R, O) ->
	table_html(R, O, []).
table_html(R, O, []) ->
	table_html(R, O, ["<table cellspacing=0>"]);
table_html({_, Staff, Chars, [{S, A}|Rest]}, Orig, Table) ->
	[Amain] = [X || #{<<"main_alias">> := X, <<"id">> := S1} <- Staff, S1 == S],
	T = table_html_chars({[], Staff, Chars}, Amain, A, Orig, Table ++ ["<tr class=staff><td colspan=2><a href=\"https://vndb.org/s", ht(integer_to_binary(S)), "\">", ht(staff_name(Staff, S, Orig)), "</a></td></tr>"]),
	table_html({[], Staff, Chars, Rest}, Orig, T);
table_html({_, _, _, []}, _, Table) ->
	Table ++ ["</table>"].
table_html_chars(D, Amain, [{A, C}|Rest], Orig, Table) ->
	T = table_html_chars_(D, Amain, A, C, Orig, Table),
	table_html_chars(D, Amain, Rest, Orig, T);
table_html_chars(_, _, [], _, Table) ->
	Table.
table_html_chars_(D = {_, Staff, Chars}, Amain, A, [C|Rest], Orig, Table) ->
	table_html_chars_(D, Amain, A, Rest, Orig, Table ++ [
		"<tr><td><a href=\"https://vndb.org/c",
		ht(integer_to_binary(C)), "\">",
		ht(char_name(Chars, C, Orig)),
		"</a></td><td>",
		case Amain == A of
			true -> "";
			false -> ht(alias_name(Staff, A, Orig))
		end,
		"</td></tr>"]);
table_html_chars_(_, _, _, [], _, Table) ->
	Table.

q(S, _, Input) when Input /= "" ->
	Options = string:slice(Input, 0, 4),
	Query = string:slice(Input, 4, infinity),
	<<UserList:1, OrigNames:1, _/bits>> = base64:decode(Options),
	IDs = case bool(UserList) of
		% true -> TODO;
		false -> [list_to_integer(X) || X <- string:split(Query, ",", all)]
	end,
	seiyuu_vndb ! {query, self(), IDs},
	receive Results -> Results end,
	
	mod_esi:deliver(S, ["Content-type: text/html\r\n\r\n", "<head><style>tr.staff { margin-left: 2em; } tr:not(.staff) > td { padding-left: 2em; } td { min-width: 30em; padding: 0.1em 1em; } tr:not(.staff):nth-of-type(2n) { background-color: #333; } tr:not(.staff):nth-of-type(2n-1) { background-color: #393939; } body { background-color: #111; color: #909090; font-family: PC9800, VGA, MS Gothic, sans-serif; } a { text-decoration: none; color: #7bd }</style></head><body>", table_html(Results, bool(OrigNames))]);
q(S, _, "") ->
	mod_esi:deliver(S, ["Content-type: text/html\r\n\r\n", <<"にゃあ"/utf8>>]).
