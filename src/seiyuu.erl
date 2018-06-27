-module(seiyuu).
-export([start/0, loop/2]).
-export([index/3, main/3, get/3, q/3]).
-import(seiyuu_util, [bool/1, ht/1, uri_decode/1]).

-define(BOILERPLATE, "<!doctype html><html><head><style>table { width: 75%; margin-left: auto; margin-right: auto; } tr.staff { margin-left: 2em; } tr:not(.staff) > td { padding-left: 2em; } td { padding: 0.1em 1em; } tr:not(.staff):nth-of-type(2n) { background-color: #181818; } tr:not(.staff):nth-of-type(2n-1) { background-color: #1e1e1e; } #mainsearch { float: right; margin-top: auto; margin-bottom: auto; margin-left: auto; } #header { display: flex; margin: 0; padding: 0.1em 0.5em; background-color: #050505; } #header > a[target=main] { font-size: 2em; } body { margin: 0; background-color: #111; color: #909090; font-family: PC9800, VGA, sans-serif; } a { text-decoration: none; color: #7bd } iframe { width: 100%; height: 80vh; border: none; }</style></head><body>").

start() ->
	% maybe TODO: lazy login
	% TODO: read values below from a config file (cache too?)
	Auth = [{protocol, 1}, {client, <<"test">>}, {clientver, <<"0.1">>}],
	V = vndb:connect(),
	ok = vndb:login(V, Auth),
	Vp = spawn(seiyuu, loop, [V, Auth]),   register(seiyuu_vndb, Vp),
	Cp = spawn(seiyuu_cache, loop, [#{}]), register(seiyuu_cache, Cp),
	ok.

loop(V, Auth) ->
	receive
		{query, PID, IDs} ->
			% TODO: don't fail completely when vndb isn't reachable,
			% just return some error with cached results
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
data_name(Data, ID) ->
	#{ID := #{<<"name">> := Romaji, <<"original">> := Original}} = Data,
	{Romaji, case Original of null -> Romaji; _ -> Original end}.
alias_name(Data, ID) ->
	[{Romaji, Original}] = [{Romaji, Original} || #{<<"aliases">> := Aliases} <- maps:values(Data), [AID, Romaji, Original] <- Aliases, AID == ID],
	{Romaji, case Original of null -> Romaji; _ -> Original end}.
data_title(Data, ID) ->
	#{ID := #{<<"title">> := Romaji, <<"original">> := Original}} = Data,
	{Romaji, case Original of null -> Romaji; _ -> Original end}.

% (cosmetic) TODO: don't include alt if alt == name
vndb_link(Prefix, ID, {Name, Alt}, Orig) when Orig == false ->
	["<a href=\"https://vndb.org/", Prefix, ht(integer_to_binary(ID)), "\" title=\"", ht(Alt), "\">", ht(Name), "</a>"];
vndb_link(Prefix, ID, {Name, Alt}, Orig) when Orig == true ->
	vndb_link(Prefix, ID, {Alt, Name}, false).
vndb_alias({Name, Alt}, Orig) when Orig == false ->
	["<span title=\"", ht(Alt), "\">", ht(Name), "</span>"];
vndb_alias({Name, Alt}, Orig) when Orig == true ->
	vndb_alias({Alt, Name}, false).

table_html(R, O) ->
	table_html(R, O, []).
table_html(R, O, []) ->
	table_html(R, O, ["<table cellspacing=0>"]);
table_html({{VNs, Staff, Chars, [{S, A}|Rest]}, IDs}, Orig, Table) ->
	#{S := #{<<"main_alias">> := Amain}} = Staff,
	T = table_html_chars({VNs, Staff, Chars}, IDs, Amain, A, Orig, Table ++ ["<tr class=staff><td colspan=2>", vndb_link("s", S, data_name(Staff, S), Orig), "</td></tr>"]),
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
		"<tr><td>",
		vndb_link("c", C, data_name(Chars, C), Orig),
		"</td><td>",
		case Amain == A of
			true -> "";
			false -> vndb_alias(alias_name(Staff, A), Orig)
		end,
		"</td><td>",
		lists:join(", ", lists:usort([vndb_link("v", V, data_title(VNs, V), Orig) || V <- char_vns(Chars, C), lists:member(V, IDs)])),
		"</td></tr>"]);
table_html_chars_(_, _, _, _, [], _, Table) ->
	Table.

query_ids(false, Query) ->
	[list_to_integer(X) || X <- string:split(Query, ",", all)];
query_ids(true, Query) ->
	seiyuu_vndb ! {vnlist, self(), list_to_integer(Query)},
	receive {vnlist, IDs} -> IDs end.

index(S, _, _) ->
	mod_esi:deliver(S, ["Content-type: text/html; charset=utf-8\r\n\r\n", ?BOILERPLATE,
	"<div id=header><a target=main href=main>", <<"声優"/utf8>>, "</a>",
	"<form target=main id=mainsearch action=get method=GET>",
		"<input name=ids placeholder=5,13774 />",
		"<select name=orig>",
			"<option value=0>Romanized names</option>",
			"<option value=1>Original names</option>",
		"</select><select name=user>",
			"<option value=0>VNs</option>",
			"<option value=1>User</option>",
		<<"</select><input type=submit value=→ />"/utf8>>,
	"(browse <a href=\"https://vndb.org/v/all\">VNs</a>/",
	"<a href=\"https://vndb.org/u/all\">users</a>)</form></div><div>",
	"<iframe name=main src=\"about:blank\" /></div>"]).
main(S, _, _) ->
	mod_esi:deliver(S, ["Content-type: text/html; charset=utf-8\r\n\r\n",
		?BOILERPLATE ]).
get(S, _, Input) ->
	Q = httpd:parse_query(Input),
	% TODO: strip whitespace (all whitespace, everywhere, not just ^ and $)
	IDs = proplists:get_value("ids", Q),
	Orig = list_to_integer(proplists:get_value("orig", Q, 1)) * 65248,
	User = list_to_integer(proplists:get_value("user", Q, 0)),
	% TODO: uri_encode                           ↓ (no utf-8 aware uri_encode in stdlib)
	mod_esi:deliver(S, "Location: q/" ++ [118 + Orig - User] ++ IDs ++ "\r\n\r\n").
q(S, _, Input) ->
	[Mode|Query] = uri_decode(Input),
	OrigNames = Mode > 255,
	UserList = OrigNames and (Mode - 65248 == 117) orelse Mode == 117,
	true = UserList or (Mode - 65248 == 118) orelse Mode == 118,

	IDs = query_ids(UserList, Query),
	seiyuu_vndb ! {query, self(), IDs},
	receive {query, Results} -> Results end,

	Response = ["Content-type: text/html; charset=utf-8\r\n\r\n", ?BOILERPLATE, table_html({Results, IDs}, OrigNames)],
	% ugly workaround, for some reason lists:flatten/1 (which is used
	% internally by mod_esi:deliver/2) fails on certain large iolists,
	% but iolist_to_binary/1 doesn't
	mod_esi:deliver(S, binary_to_list(iolist_to_binary(Response))).
