-module(seiyuu_vndb).
-export([loop/2, get_all/3, get_all/4]).
-include_lib("kernel/include/logger.hrl").

% this process is used for communicating with VNDB, we wouldn't
% be able to rate limit multiple simultaneous queries without one

loop(V, Auth) ->
	receive
		{get, PID, Type, Flags, Filters, Options} ->
			{Res, Results} = vndb:get(V, Type, Flags, Filters, Options),
			case Res of
				error ->
					#{<<"id">> := <<"throttled">>, <<"fullwait">> := Timeout} = Results,
					?LOG_INFO("throttled (~ps)~n", [Timeout]),
					timer:sleep(timer:seconds(ceil(Timeout))),
					self() ! {get, PID, Type, Flags, Filters, Options};
				results ->
					PID ! {get, Results}
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
	seiyuu_vndb ! {get, self(), Type, Flags, Filters, Options},
	receive {get, #{<<"more">> := More, <<"items">> := NewItems}} -> ok end,
	case More of
		false -> Items ++ NewItems;
		true -> get_all_(Type, Flags, Filters, Options#{page => Page+1}, Items ++ NewItems)
	end.
