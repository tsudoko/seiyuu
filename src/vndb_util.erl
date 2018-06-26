-module(vndb_util).
-export([get_all/4, get_all/5]).

get_all(V, Type, Flags, Filters) ->
	get_all(V, Type, Flags, Filters, #{}).
get_all(V, Type, Flags, Filter, Options) ->
	get_all(V, Type, Flags, Filter, Options, []).
get_all(V, Type, Flags, Filter, Options, Items) ->
	Page = maps:get(page, Options, 1),
	{Response, R} = vndb:get(V, Type, Flags, Filter, Options),
	case Response of
		error ->
			#{<<"id">> := <<"throttled">>, <<"fullwait">> := Timeout} = R,
			logger:info("throttled (~fs)~n", [Timeout]),
			timer:sleep(timer:seconds(ceil(Timeout))),
			get_all(V, Type, Flags, Filter, Options, Items);
		results ->
			#{<<"more">> := More, <<"items">> := NewItems} = R,
			case More of
				false -> Items ++ NewItems;
				true -> get_all(V, Type, Flags, Filter, Options#{page => Page+1}, Items ++ NewItems)
			end
	end.