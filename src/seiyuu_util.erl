-module(seiyuu_util).
-export([bool/1, ht/1, uri_decode/1, idmap/1]).

bool(0) -> false;
bool(N) when N+1 == N+1 -> true.
ht(Text) -> re:replace(re:replace(re:replace(Text, "<", "&lt;"), ">", "&gt;"), "&", "&amp;").
uri_decode(S) -> unicode:characters_to_list(list_to_binary(http_uri:decode(S))).
idmap(List) -> maps:from_list([{ID, Data} || #{<<"id">> := ID} = Data <- List]).
