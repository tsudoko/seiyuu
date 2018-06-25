-module(seiyuu_util).
-export([bool/1, ht/1]).

bool(0) -> false;
bool(N) when N+1 == N+1 -> true.
ht(Text) -> re:replace(re:replace(re:replace(Text, "<", "&lt;"), ">", "&gt;"), "&", "&amp;").
