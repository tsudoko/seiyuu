-module(seiyuu_vndb).
-export([loop/2, get_all/3, get_all/4]).
-include_lib("kernel/include/logger.hrl").

loop(V, Auth) ->
	receive
		{get, PID, V, Type, Flags, Filters, Options} ->
			{Response, R} = vndb:get(V, Type, Flags, Filters, Options),
			case Response of
				error ->
					#{<<"id">> := <<"throttled">>, <<"fullwait">> := Timeout} = R,
					?LOG_INFO("throttled (~fs)~n", [Timeout]),
					timer:sleep(timer:seconds(ceil(Timeout))),
					self() ! {get, PID, V, Type, Flags, Filters, Options};
				results ->
					PID ! {get, Response}
			end;
		Msg ->
			throw({unknown_msg, Msg})
	end,
	loop(V, Auth).

get_all(Type, Flags, Filters) ->
	get_all(Type, Flags, Filters, #{}).
get_all(Type, Flags, Filters, Options) ->
	get_all_(Type, Flags, Filters, Options, []).
get_all_(Type, Flags, Filters, Options, Items) ->
	Page = maps:get(page, Options, 1),
	seiyuu_vndb ! {get, Type, Flags, Filters, Options, Items},
	receive {get, #{<<"more">> := More, <<"items">> := NewItems}} -> ok end,
	case More of
		false -> Items ++ NewItems;
		true -> get_all_(Type, Flags, Filters, Options#{page => Page+1}, Items ++ NewItems)
	end.
