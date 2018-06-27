-module(seiyuu_cache).
-export([loop/1, get/5]).

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
loop(Caches) ->
	receive
		{cacheload, NewCaches} ->
			loop(NewCaches);
		{cachedump, PID} ->
			PID ! {cachedump, Caches},
			loop(Caches);
		{cacheget, PID, Type, IDParam, IDs} ->
			CacheDir = maps:get(Type, Caches, #{IDParam => #{}}),
			#{IDParam := Cache} = CacheDir,
			Uncached = [ID || ID <- IDs, not maps:is_key(ID, Cache)],
			Data = maps:with(IDs, Cache),
			PID ! {cacheget, Uncached, Data},
			loop(Caches);
		{cacheput, Type, IDParam, NewData} ->
			CacheDir = maps:get(Type, Caches, #{IDParam => #{}}),
			#{IDParam := Cache} = CacheDir,
			loop(Caches#{Type => #{IDParam => maps:merge(Cache, NewData)}});
		Msg ->
			throw({unknown_msg, Msg}),
			loop(Caches)
	end.

% this function assumes Flags are always the same for a given type
get(V, Type, Flags, IDParam, IDs) ->
	seiyuu_cache ! {cacheget, self(), Type, IDParam, IDs},
	receive {cacheget, Uncached, Cached} -> ok end,
	maps:merge(Cached, request_uncached(V, Type, Flags, IDParam, Uncached)).

request_uncached(_, _, _, _, []) ->
	#{};
request_uncached(V, Type = vnlist, Flags, IDParam = "uid", [ID]) ->
	response_to_map_(Type, IDParam, [ID], vndb_util:get_all(V, Type, Flags, ["(", IDParam, " = ", integer_to_binary(ID), ")"]));
request_uncached(V, Type, Flags, IDParam, IDs) ->
	R = case IDs of
		[ID] -> vndb_util:get_all(V, Type, Flags, ["(", IDParam, " = ", integer_to_binary(ID), ")"]);
		_ ->    vndb_util:get_all(V, Type, Flags, ["(", IDParam, " = [", lists:join(",", [integer_to_binary(X) || X <- IDs]), "])"])
	end,
	RMap = response_to_map_(Type, IDParam, IDs, R),
	seiyuu_cache ! {cacheput, Type, IDParam, RMap},
	RMap.
response_to_map_(_, "id", _, R) ->
	maps:from_list([{ID, Data} || #{<<"id">> := ID} = Data <- R]);
response_to_map_(vnlist, "uid", [ID], R) ->
	#{ID => R};
response_to_map_(character, "vn", IDs, R) ->
	maps:from_list([{VNID, [Data || #{<<"vns">> := VNs} = Data <- R, [ID|_] <- VNs, ID == VNID]} || VNID <- IDs]).
