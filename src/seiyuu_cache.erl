-module(seiyuu_cache).
-export([loop/1, get/4]).

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
			DMap = maps:from_list([{ID, Data} || #{<<"id">> := ID} = Data <- NewData]),
			loop(Caches#{Type => maps:merge(Cache, DMap)}, Relations);
		{cacheput, Type, "vn", IDs, NewData} ->
			Rel = maps:get({"vn", Type}, Relations, #{}),
			NewRel = [{VNID, [ID || #{<<"id">> := ID, <<"vns">> := VNs} <- NewData, [VNID|_] <- VNs, VNID == ID] || VNID <- IDs}],
			self() ! {cacheput, Type, "id", IDs, NewData},
			loop(Caches, Relations#{{"vn", Type} => maps:merge(Rel, NewRel)});
		Msg ->
			throw({unknown_msg, Msg}),
			loop(Caches, Relations)
	end.

% this function assumes Flags are always the same for a given type
% TODO: don't fail completely when vndb isn't reachable, just return
% some error with cached results
get(Type, Flags, IDParam, IDs) ->
	seiyuu_cache ! {cacheget, self(), Type, IDParam, IDs},
	receive {cacheget, Uncached, Cached} -> ok end,
	maps:merge(Cached, request_uncached(Type, Flags, IDParam, Uncached)).

request_uncached(_, _, _, []) ->
	#{};
request_uncached(Type = vnlist, Flags, IDParam = "uid", [ID]) ->
	response_to_map_(Type, IDParam, [ID], seiyuu_vndb:get_all(Type, Flags, ["(", IDParam, " = ", integer_to_binary(ID), ")"]));
request_uncached(Type, Flags, IDParam, IDs) ->
	R = case IDs of
		[ID] -> seiyuu_vndb:get_all(Type, Flags, ["(", IDParam, " = ", integer_to_binary(ID), ")"]);
		_ ->    seiyuu_vndb:get_all(Type, Flags, ["(", IDParam, " = [", lists:join(",", [integer_to_binary(X) || X <- IDs]), "])"])
	end,
	seiyuu_cache ! {cacheput, Type, IDParam, IDs, R},
	R.
