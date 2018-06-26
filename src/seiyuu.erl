-module(seiyuu).
-export([start/0, loop/3, query/3]).
-export([q/3]).
-import(seiyuu_util, [bool/1, ht/1]).

start() ->
	% maybe TODO: lazy login
	Auth = [{protocol, 1}, {client, <<"test">>}, {clientver, <<"0.1">>}],
	V = vndb:connect(),
	vndb:login(V, Auth),
	PID = spawn(seiyuu, loop, [V, Auth, #{}]),
	register(seiyuu_vndb, PID),
	ok.

% would be less hacky to just recreate an API server locally probably,
% but that's too much work
%
% i.e. right now only exact requests are cached, so if you do "get
% character (vn = 1)" and it returns characters 100, 101, 102, "get
% character (id = [100,101,102])" won't use the cache even though the
% data is all there
%
% this is not an issue as long as we search by the same field in
% every request for a given data type, though, and that's what we
% do currently
loop(V, Auth, Caches) ->
	receive
		{cacheload, NewCaches} ->
			loop(V, Auth, NewCaches);
		{cachedump, PID} ->
			PID ! {cachedump, Caches},
			loop(V, Auth, Caches);
		{cacheget, PID, Type, IDParam, IDs} ->
			CacheDir = maps:get(Type, Caches, #{IDParam => #{}}),
			#{IDParam := Cache} = CacheDir,
			Uncached = [ID || ID <- IDs, not maps:is_key(ID, Cache)],
			Data = maps:with(IDs, Cache),
			PID ! {cacheget, Uncached, Data},
			loop(V, Auth, Caches);
		{cacheput, Type, IDParam, NewData} ->
			CacheDir = maps:get(Type, Caches, #{IDParam => #{}}),
			#{IDParam := Cache} = CacheDir,
			loop(V, Auth, Caches#{Type => #{IDParam => maps:merge(Cache, NewData)}});
		{query, PID, IDs} ->
			spawn(seiyuu, query, [V, IDs, PID]),
			loop(V, Auth, Caches)
		% TODO: crash on unknown messages
	end.

% this function assumes Flags are always the same for a given type
get(V, Type, Flags, IDParam, IDs) ->
	seiyuu_vndb ! {cacheget, self(), Type, IDParam, IDs},
	receive {cacheget, Uncached, Cached} -> ok end,
	maps:merge(Cached, request_uncached(V, Type, Flags, IDParam, Uncached)).

request_uncached(_, _, _, _, []) ->
	#{};
request_uncached(V, Type, Flags, IDParam, IDs) ->
	R = case IDs of
		[ID] -> vndb_util:get_all(V, Type, Flags, ["(", IDParam, " = ", integer_to_binary(ID), ")"]);
		_ ->    vndb_util:get_all(V, Type, Flags, ["(", IDParam, " = [", lists:join(",", [integer_to_binary(X) || X <- IDs]), "])"])
	end,
	RMap = response_to_map_(Type, IDParam, IDs, R),
	seiyuu_vndb ! {cacheput, Type, IDParam, RMap},
	RMap.
response_to_map_(_, "id", _, R) ->
	maps:from_list([{ID, Data} || #{<<"id">> := ID} = Data <- R]);
response_to_map_(character, "vn", IDs, R) ->
	maps:from_list([{VNID, [Data || #{<<"vns">> := VNs} = Data <- R, [ID|_] <- VNs, ID == VNID]} || VNID <- IDs]).

query(V, IDs, PID) ->
	% TODO: sort by vn
	VNs = get(V, vn, [basic], "id", IDs),
	Chars = maps:from_list([{CharID, Data} || #{<<"id">> := CharID} = Data <- lists:flatten(maps:values(get(V, character, [basic, voiced, vns], "vn", IDs)))]),
	Staff = get(V, staff, [basic, aliases], "id",
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
	PID ! {query, {VNs, Staff, Chars, StaffChars}}.

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

q(S, _, Input) when Input /= "" ->
	Options = string:slice(Input, 0, 4),
	Query = string:slice(Input, 4, infinity),
	<<UserList:1, OrigNames:1, _/bits>> = base64:decode(Options),
	IDs = case bool(UserList) of
		% true -> TODO;
		false -> [list_to_integer(X) || X <- string:split(Query, ",", all)]
	end,
	seiyuu_vndb ! {query, self(), IDs},
	receive {query, Results} -> Results end,

	Response = ["Content-type: text/html; charset=utf-8\r\n\r\n", "<head><style>table { width: 75%; margin-left: auto; margin-right: auto; } tr.staff { margin-left: 2em; } tr:not(.staff) > td { padding-left: 2em; } td { padding: 0.1em 1em; } tr:not(.staff):nth-of-type(2n) { background-color: #181818; } tr:not(.staff):nth-of-type(2n-1) { background-color: #1e1e1e; } body { background-color: #111; color: #909090; font-family: PC9800, VGA, sans-serif; } a { text-decoration: none; color: #7bd }</style></head><body>", table_html({Results, IDs}, bool(OrigNames))],
	% ugly workaround, for some reason lists:flatten/1 (which is used
	% internally by mod_esi:deliver/2) fails on certain large iolists,
	% but iolist_to_binary/1 doesn't
	mod_esi:deliver(S, binary_to_list(iolist_to_binary(Response)));
q(S, _, "") ->
	mod_esi:deliver(S, ["Content-type: text/html; charset=utf-8\r\n\r\n", <<"にゃあ"/utf8>>]).
