-module(seiyuu_cache).
-export([loop/1, get/4]).
-import(seiyuu_util, [idmap/1]).

% the cache assumes Flags passed to get/4 are always the same
% for a given type

loop(Caches, Relations) ->
	receive
		{cacheload, {NewCaches, NewRelations}} ->
			loop(NewCaches, NewRelations);
		{cachedump, PID} ->
			PID ! {cachedump, {Caches, Relations}},
			loop(Caches, Relations);
		{cacheget, PID, Type, "id", IDs} ->
			Cache = maps:get(Type, Caches, #{}),
			Uncached = [ID || ID <- IDs, not maps:is_key(ID, Cache)],
			Data = maps:values(maps:with(IDs, Cache)),
			PID ! {cacheget, Uncached, Data},
			loop(Caches, Relations);
		{cacheget, PID, Type, "vn", RelIDs} ->
			Cache = maps:get(Type, Caches, #{}),
			Rel = maps:get({"vn", Type}, Relations, #{}),
			Uncached = [ID || ID <- RelIDs, not maps:is_key(ID, Rel)],
			CachedIDs = lists:flatten(maps:values(maps:with(IDs, Rel))),
			Data = maps:values(maps:with(CachedIDs, Cache)),
			PID ! {cacheget, Uncached, Data},
			loop(Caches, Relations);
		{cacheput, Type, "id", _, NewData} ->
			Cache = maps:get(Type, Caches, #{}),
			loop(Caches#{Type => maps:merge(Cache, idmap(NewData))}, Relations);
		{cacheput, Type, "vn", IDs, NewData} ->
			Rel = maps:get({"vn", Type}, Relations, #{}),
			NewRel = maps:from_list([{VNID, [ID || #{<<"id">> := ID, <<"vns">> := VNs} <- NewData, [VNID|_] <- VNs, VNID == ID] || VNID <- IDs}]),
			self() ! {cacheput, Type, "id", IDs, NewData},
			loop(Caches, Relations#{{"vn", Type} => maps:merge(Rel, NewRel)});
		Msg ->
			throw({unknown_msg, Msg}),
			loop(Caches, Relations)
	end.

% TODO: don't fail completely when vndb isn't reachable, just return
% some error with cached results

% special case - never cached
get(Type = vnlist, Flags, IDParam = "uid", IDs) ->
	request_uncached(Type, Flags, IDParam, IDs);
get(Type, Flags, IDParam, IDs) ->
	seiyuu_cache ! {cacheget, self(), Type, IDParam, IDs},
	receive {cacheget, Uncached, Cached} -> ok end,
	NewData = request_uncached(Type, Flags, IDParam, Uncached),
	seiyuu_cache ! {cacheput, Type, IDParam, IDs, NewData},
	maps:merge(Cached, NewData).

request_uncached(_, _, _, []) ->
	[];
% some get commands don't accept ID arrays, but work fine with a single ID
request_uncached(Type, Flags, IDParam, [ID]) ->
	seiyuu_vndb:get_all(Type, Flags, ["(", IDParam, " = ", integer_to_binary(ID), ")"]);
request_uncached(Type, Flags, IDParam, IDs) ->
	seiyuu_vndb:get_all(Type, Flags, ["(", IDParam, " = [", lists:join(",", [integer_to_binary(X) || X <- IDs]), "])"]).
