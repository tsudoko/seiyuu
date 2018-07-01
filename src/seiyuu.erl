-module(seiyuu).
-export([start/0]).
-export([vnlist/2, query/2]).
-export([index/3, main/3, get/3, q/3]).
-import(seiyuu_util, [bool/1, ht/1, uri_decode/1, idmap/1]).
-export([staffchars/3]).

-define(BOILERPLATE, <<"<!doctype html><html><head><style>table { width: 75%; margin-left: auto; margin-right: auto; } tr.staff { margin-left: 2em; } tr:not(.staff) > td { padding-left: 2em; } td { padding: 0.1em 1em; } tr:not(.staff):nth-of-type(2n) { background-color: #181818; } tr:not(.staff):nth-of-type(2n-1) { background-color: #1e1e1e; } #mainsearch { float: right; margin-top: auto; margin-bottom: auto; margin-left: auto; } #header { display: flex; margin: 0; padding: 0.1em 0.5em; background-color: #050505; } #header > a[target=main] { font-size: 2em; } body { margin: 0; background-color: #111; color: #909090; font-family: PC9800, VGA, sans-serif; } a { text-decoration: none; color: #7bd } iframe { width: 100%; height: 80vh; border: none; }</style></head><body>"/utf8>>).

start() ->
	% maybe TODO: lazy login
	% TODO: read values below from a config file (cache too?)
	Auth = [{protocol, 1}, {client, <<"test">>}, {clientver, <<"0.1">>}],
	V = vndb:connect(),
	ok = vndb:login(V, Auth),
	Vp = spawn(seiyuu_vndb, loop, [V, Auth]), register(seiyuu_vndb, Vp),
	Cp = spawn(seiyuu_cache, loop, [#{}, #{}]), register(seiyuu_cache, Cp),
	ok.

staffchars(Chars, Staff, VNs) ->
	StaffIDs = [ID || #{<<"id">> := ID} <- Staff],
	VNIDs = [ID || #{<<"id">> := ID} <- VNs],
	staffchars_(Chars, {StaffIDs, VNIDs}, #{}).
staffchars_([#{<<"id">> := CID, <<"voiced">> := Voiced}|Rest], IDs, R) ->
	staffchars_(Rest, IDs, staffchars_char(CID, Voiced, IDs, R));
staffchars_([], _, R) ->
	R.
staffchars_char(CID, [V|Rest], {StaffIDs, VNIDs}, R) ->
	#{<<"id">> := SID, <<"vid">> := VID, <<"aid">> := AID} = V,
	SR = maps:get(SID, R, #{}),
	{AIDs, VIDs} = CR = maps:get(CID, SR, {[], []}),
	NewCR = case {lists:member(SID, StaffIDs), lists:member(VID, VNIDs)} of
		{true, true} ->	{[AID|AIDs], [VID|VIDs]};
		_ -> CR
	end,
	staffchars_char(CID, Rest, {StaffIDs, VNIDs}, R#{SID => SR#{CID => NewCR}});
staffchars_char(_, [], _, R) ->
	R.

vnlist(PID, UID) ->
	PID ! {self(), [ID || #{<<"vn">> := ID} <- seiyuu_cache:get(vnlist, [basic], "uid", [UID])]}.
query(PID, IDs) ->
	% TODO: sort by vn
	%   ↑   seems a bit complex though, we'd need to sort all the keys
	%       (staff, char, alias) and then sort each alias by VNs too
	VNs = seiyuu_cache:get(vn, [basic], "id", IDs),
	Chars = seiyuu_cache:get(character, [basic, voiced, vns], "vn", IDs),
	Staff = seiyuu_cache:get(staff, [basic, aliases], "id",
		lists:usort([ID || #{<<"id">> := ID} <- lists:flatten([V || #{<<"voiced">> := V} <- Chars])])),
	PID ! {self(), {idmap(VNs), idmap(Staff), idmap(Chars), staffchars(Chars, Staff, VNs)}}.

% --- html stuff below

char_vns(Chars, ID) ->
	#{ID := #{<<"vns">> := VNs}} = Chars,
	[V || [V|_] <- VNs].
data_name(Data, ID) ->
	#{ID := #{<<"name">> := Romaji, <<"original">> := Original}} = Data,
	{Romaji, Original}.
alias_name(Data, ID) ->
	[{Romaji, Original}] = [{Romaji, Original} || #{<<"aliases">> := Aliases} <- maps:values(Data), [AID, Romaji, Original] <- Aliases, AID == ID],
	{Romaji, Original}.
data_title(Data, ID) ->
	#{ID := #{<<"title">> := Romaji, <<"original">> := Original}} = Data,
	{Romaji, Original}.

vndb_link(Prefix, ID, {Name, null}, _) ->
	["<a href=\"https://vndb.org/", Prefix, ht(integer_to_binary(ID)), "\">", ht(Name), "</a>"];
vndb_link(Prefix, ID, {Name, Alt}, _Orig = false) ->
	["<a href=\"https://vndb.org/", Prefix, ht(integer_to_binary(ID)), "\" title=\"", ht(Alt), "\">", ht(Name), "</a>"];
vndb_link(Prefix, ID, {Name, Alt}, _Orig = true) ->
	vndb_link(Prefix, ID, {Alt, Name}, false).
vndb_alias({Name, null}, _) ->
	ht(Name);
vndb_alias({Name, Alt}, _Orig = false) ->
	["<span title=\"", ht(Alt), "\">", ht(Name), "</span>"];
vndb_alias({Name, Alt}, _Orig = true) ->
	vndb_alias({Alt, Name}, false).

table_html(R, O, Send) ->
	Send("<table cellspacing=0>"),
	table_html_staff(R, O, Send).
table_html_staff({{VNs, Staff, Chars, [{S, A}|Rest]}, IDs}, Orig, Send) ->
	#{S := #{<<"main_alias">> := Amain}} = Staff,
	Send("<tr class=staff><td colspan=2>"),
	Send(vndb_link("s", S, data_name(Staff, S), Orig)),
	Send("</td></tr>"),
	table_html_aliases({VNs, Staff, Chars}, IDs, Amain, A, Orig, Send),
	table_html_staff({{VNs, Staff, Chars, Rest}, IDs}, Orig, Send);
table_html_staff({{_, _, _, []}, _}, _, Send) ->
	Send("</table>").
table_html_aliases(D, IDs, Amain, [{A, C}|Rest], Orig, Send) ->
	table_html_chars(D, IDs, Amain, A, C, Orig, Send),
	table_html_aliases(D, IDs, Amain, Rest, Orig, Send);
table_html_aliases(_, _, _, [], _, _) -> ok.
table_html_chars(D = {VNs, Staff, Chars}, IDs, Amain, A, [C|Rest], Orig, Send) ->
	Send("<tr><td>"),
	Send(vndb_link("c", C, data_name(Chars, C), Orig)),
	Send("</td><td>"),
	Send(case Amain == A of true -> ""; false -> vndb_alias(alias_name(Staff, A), Orig) end),
	Send("</td><td>"),
	Send(lists:join(", ", lists:usort([vndb_link("v", V, data_title(VNs, V), Orig) || V <- char_vns(Chars, C), lists:member(V, IDs)]))),
	Send("</td></tr>"),
	table_html_chars(D, IDs, Amain, A, Rest, Orig, Send);
table_html_chars(_, _, _, _, [], _, _) -> ok.

query_ids(_UserList = false, Query) ->
	[list_to_integer(X) || X <- string:split(Query, ",", all)];
query_ids(_UserList = true, Query) ->
	PID = spawn(?MODULE, vnlist, [self(), list_to_integer(Query)]),
	receive {PID, VNList} -> VNList end.

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
	Orig = list_to_integer(proplists:get_value("orig", Q, 1)) * $ﻠ,
	User = list_to_integer(proplists:get_value("user", Q, 0)),
	% TODO: uri_encode                           ↓ (no utf-8 aware uri_encode in stdlib)
	mod_esi:deliver(S, "Location: q/" ++ [$v + Orig - User] ++ IDs ++ "\r\n\r\n").
q(S, _, Input) ->
	[Mode|Query] = uri_decode(Input),
	OrigNames = Mode > $ﻠ,
	UserList = OrigNames and (Mode - $ﻠ == $u) orelse Mode == $u,
	true = UserList or (Mode - $ﻠ == $v) orelse Mode == $v,
	IDs = query_ids(UserList, Query),
	QueryPid = spawn(?MODULE, query, [self(), IDs]),
	receive {QueryPid, Results} -> ok end,

	mod_esi:deliver(S, "Content-type: text/html; charset=utf-8\r\n\r\n"),
	mod_esi:deliver(S, ?BOILERPLATE),
	table_html({Results, IDs}, OrigNames, fun(R) -> mod_esi:deliver(S, R) end).
