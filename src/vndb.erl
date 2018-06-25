-module(vndb).
-export([connect/0, connect/3, cmd/2, close/1]).
-export([login/2, dbstats/1, get/4, get/5]).
-export([get_all/4, get_all/5]).
-export([nyaa/1]).

% TODO: timeouts? send/2 and recv/2 have them set to infinity

% --- basic interface

connect() -> connect("api.vndb.org", 19535, true).
connect(Host, Port, SSL) ->
	true = jsx:maps_support(),
	Opts = [binary, {packet, 0}, {active, false}],
	case SSL of
		false ->
			{ok, Sock} = gen_tcp:connect(Host, Port, Opts),
			{vndb, Sock, fun gen_tcp:send/2, fun gen_tcp:recv/2, fun gen_tcp:close/1};
		true ->
			{ok, Sock} = ssl:connect(Host, Port, Opts),
			{vndb, Sock, fun ssl:send/2, fun ssl:recv/2, fun ssl:close/1}
	end.

cmd({vndb, S, Send, Recv, _}, Cmd) ->
	ok = Send(S, [Cmd, <<4>>]),
	{ok, R} = Recv(S, 0),

	% according to https://vndb.org/d11#2 there's no strictly defined format for
	% responses, but right now they all follow a single convention: name,
	% optionally followed by a single space and JSON content, terminated with \x04
	case cmd_restype(R) of
		%{nospace, noterm} -> TODO
		{nospace, Term} ->
			<<Name:Term/binary, 4>> = R,
			binary_to_atom(Name, utf8);
		{Space, noterm} ->
			<<Name:Space/binary, " ", Rest>> = R,
			{incomplete, F} = jsx:decode(Rest, [stream, return_maps]),
			{binary_to_atom(Name, utf8), cmd_recvmore(S, Recv, F)};
		{Space, Term} ->
			Restlen = Term - Space - 1,
			<<Name:Space/binary, " ", Rest:Restlen/binary, 4>> = R,
			{binary_to_atom(Name, utf8), jsx:decode(Rest, [return_maps])}
	end.

cmd_restype(R) ->
	Match = binary:match(R, <<" ">>),
	Space = case Match of
		nomatch -> nospace;
		{Start, _} -> Start
	end,
	{Space, cmd_term(R)}.

cmd_term(R) ->
	Rlen = byte_size(R) - 1,
	case R of
		<<_:Rlen/binary, 4>> -> Rlen;
		_Else -> noterm
	end.

cmd_recvmore(S, Recv, F) ->
	{ok, Response} = Recv(S, 0),
	case cmd_term(Response) of
		noterm ->
			{incomplete, G} = F(Response),
			cmd_recvmore(S, Recv, G);
		Term ->
			<<Rest:Term/binary, 4>> = Response,
			{incomplete, G} = F(Rest),
			G(end_stream)
	end.

close({vndb, S, _, _, Close}) -> ok = Close(S).

% --- wrapped commands

login(V, Args) -> vndb:cmd(V, [<<"login ">>, jsx:encode(Args)]).
dbstats(V) -> vndb:cmd(V, <<"dbstats">>).
get(V, Type, Flags, Filters) -> get(V, Type, Flags, Filters, #{}).
get(V, Type, Flags, Filters, Options) ->
	{results, R} = vndb:cmd(V, [
		<<"get ">>,
		atom_to_binary(Type, utf8), <<" ">>,
		lists:join(<<",">>, lists:map(fun(X) -> atom_to_binary(X, utf8) end, Flags)), <<" ">>,
		Filters, <<" ">>,
		jsx:encode(Options)
	]),
	R.

get_all(V, Type, Flags, Filters) ->
	get_all(V, Type, Flags, Filters, #{}).
get_all(V, Type, Flags, Filter, Options) ->
	get_all(V, Type, Flags, Filter, Options, []).
get_all(V, Type, Flags, Filter, Options, Items) ->
	Page = maps:get(page, Options, 1),
	R = get(V, Type, Flags, Filter, Options),
	#{<<"more">> := More, <<"items">> := NewItems} = R,
	case More of
		false -> Items ++ NewItems;
		true -> get_all(V, Type, Flags, Filter, Options#{page => Page+1}, Items ++ NewItems)
	end.
