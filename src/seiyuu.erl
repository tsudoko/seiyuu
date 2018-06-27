-module(seiyuu).
-export([start/0, loop/2]).
-export([q/3]).
-import(seiyuu_util, [bool/1, ht/1, uri_decode/1]).

start() ->
	% maybe TODO: lazy login
	% TODO: read values below from a config file (cache too?)
	Auth = [{protocol, 1}, {client, <<"test">>}, {clientver, <<"0.1">>}],
	V = vndb:connect(),
	vndb:login(V, Auth),
	Vp = spawn(seiyuu, loop, [V, Auth]),   register(seiyuu_vndb, Vp),
	Cp = spawn(seiyuu_cache, loop, [#{}]), register(seiyuu_cache, Cp),
	ok.

loop(V, Auth) ->
	receive
		{query, PID, IDs} ->
			PID ! {query, query(V, IDs)};
		{vnlist, PID, UID} ->
			PID ! {vnlist, vnlist(V, UID)};
		Msg ->
			throw({unknown_msg, Msg})
	end,
	loop(V, Auth).

vnlist(V, UID) ->
	#{UID := List} = seiyuu_cache:get(V, vnlist, [basic], "uid", [UID]),
	[ID || #{<<"vn">> := ID} <- List].
query(V, IDs) ->
	% TODO: sort by vn
	VNs = seiyuu_cache:get(V, vn, [basic], "id", IDs),
	Chars = maps:from_list([{CharID, Data} || #{<<"id">> := CharID} = Data <- lists:flatten(maps:values(seiyuu_cache:get(V, character, [basic, voiced, vns], "vn", IDs)))]),
	Staff = seiyuu_cache:get(V, staff, [basic, aliases], "id",
		lists:usort([ID || #{<<"id">> := ID} <- lists:flatten([V || #{<<"voiced">> := V} <- maps:values(Chars)])])),
	% [{staff1, [{alias1, [char1, char2...]}, {alias2...}...]}, {staff2...}...]
	StaffChars =
	[{S, AliasList} ||
		#{<<"id">> := S, <<"aliases">> := Aliases} <- maps:values(Staff),
		AliasList <- [[{A, CharList} ||
			[A|_] <- Aliases,
			CharList <- [[C ||
				#{<<"id">> := C, <<"voiced">> := Voiced} <- maps:values(Chars),
				lists:member(A, [V || #{<<"aid">> := V} <- Voiced])]],
			CharList /= []]]],
	{VNs, Staff, Chars, StaffChars}.

char_vns(Chars, ID) ->
	#{ID := #{<<"vns">> := VNs}} = Chars,
	[V || [V|_] <- VNs].
data_name(Data, ID, Orig) when Orig == false ->
	#{ID := #{<<"name">> := Name}} = Data,
	Name;
data_name(Data, ID, Orig) when Orig == true ->
	#{ID := #{<<"name">> := Fallback, <<"original">> := Name}} = Data,
	case Name of null -> Fallback; _ -> Name end.
alias_name(Data, ID, Orig) ->
	[{Name, Original}] = [{Name, Original} || #{<<"aliases">> := Aliases} <- maps:values(Data), [AID, Name, Original] <- Aliases, AID == ID],
	case Orig of
		true -> case Original of null -> Name; _ -> Original end;
		false -> Name
	end.
data_title(Data, ID, Orig) when Orig == false ->
	#{ID := #{<<"title">> := Name}} = Data,
	Name;
data_title(Data, ID, Orig) when Orig == true ->
	#{ID := #{<<"title">> := Fallback, <<"original">> := Name}} = Data,
	case Name of null -> Fallback; _ -> Name end.

table_html(R, O) ->
	table_html(R, O, []).
table_html(R, O, []) ->
	table_html(R, O, ["<table cellspacing=0>"]);
table_html({{VNs, Staff, Chars, [{S, A}|Rest]}, IDs}, Orig, Table) ->
	#{S := #{<<"main_alias">> := Amain}} = Staff,
	T = table_html_chars({VNs, Staff, Chars}, IDs, Amain, A, Orig, Table ++ ["<tr class=staff><td colspan=2><a href=\"https://vndb.org/s", ht(integer_to_binary(S)), "\">", ht(data_name(Staff, S, Orig)), "</a></td></tr>"]),
	table_html({{VNs, Staff, Chars, Rest}, IDs}, Orig, T);
table_html({{_, _, _, []}, _}, _, Table) ->
	Table ++ ["</table>"].
table_html_chars(D, IDs, Amain, [{A, C}|Rest], Orig, Table) ->
	T = table_html_chars_(D, IDs, Amain, A, C, Orig, Table),
	table_html_chars(D, IDs, Amain, Rest, Orig, T);
table_html_chars(_, _, _, [], _, Table) ->
	Table.
table_html_chars_(D = {VNs, Staff, Chars}, IDs, Amain, A, [C|Rest], Orig, Table) ->
	table_html_chars_(D, IDs, Amain, A, Rest, Orig, Table ++ [
		<<"<tr><td><a href=\"https://vndb.org/c">>,
		ht(integer_to_binary(C)), "\">",
		ht(data_name(Chars, C, Orig)),
		"</a></td><td>",
		case Amain == A of
			true -> "";
			false -> ht(alias_name(Staff, A, Orig))
		end,
		"</td><td>",
		lists:join(", ", lists:usort([["<a href=\"https://vndb.org/v", ht(integer_to_binary(X)), "\">", ht(data_title(VNs, X, Orig)), "</a>"] || X <- char_vns(Chars, C), lists:member(X, IDs)])),
		"</td></tr>"]);
table_html_chars_(_, _, _, _, [], _, Table) ->
	Table.

query_ids(false, Query) ->
	[list_to_integer(X) || X <- string:split(Query, ",", all)];
query_ids(true, Query) ->
	seiyuu_vndb ! {vnlist, self(), list_to_integer(Query)},
	receive {vnlist, IDs} -> IDs end.

q(S, _, Input) when Input /= "" ->
	[Mode|Query] = uri_decode(Input),
	OrigNames = Mode > 255,
	UserList = OrigNames and (Mode - 65248 == 117) orelse Mode == 117,
	true = UserList or (Mode - 65248 == 118) orelse Mode == 118,

	IDs = query_ids(UserList, Query),
	seiyuu_vndb ! {query, self(), IDs},
	receive {query, Results} -> Results end,

	Response = ["Content-type: text/html; charset=utf-8\r\n\r\n", "<head><style>table { width: 75%; margin-left: auto; margin-right: auto; } tr.staff { margin-left: 2em; } tr:not(.staff) > td { padding-left: 2em; } td { padding: 0.1em 1em; } tr:not(.staff):nth-of-type(2n) { background-color: #181818; } tr:not(.staff):nth-of-type(2n-1) { background-color: #1e1e1e; } body { background-color: #111; color: #909090; font-family: PC9800, VGA, sans-serif; } a { text-decoration: none; color: #7bd }</style></head><body>", table_html({Results, IDs}, OrigNames)],
	% ugly workaround, for some reason lists:flatten/1 (which is used
	% internally by mod_esi:deliver/2) fails on certain large iolists,
	% but iolist_to_binary/1 doesn't
	mod_esi:deliver(S, binary_to_list(iolist_to_binary(Response)));
q(S, _, "") ->
	mod_esi:deliver(S, ["Content-type: text/html; charset=utf-8\r\n\r\n", <<"にゃあ"/utf8>>]).
