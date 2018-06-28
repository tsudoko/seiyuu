-module(vndb_util).
-export([get_all/4, get_all/5]).

% since throttles are handled inside this function, calling it from
% more than one process at a time on the same connection might make
% yorhel angry
%
% if we wanted to fix that, we could spawn a process from vndb.erl
% and make passing messages to it the only way to interact with
% vndb; the process would handle throttles internally
%
% or handle throttling in the seiyuu_vndb process ┐(´ー｀)┌
% might be better not to do that though, get_all in its current
% state doesn't play well with our cache anyway, since you have to
% wait until you get all pages before the results get written to
% the cache
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
