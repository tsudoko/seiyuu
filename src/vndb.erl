-module(vndb).
-export([connect/0, connect/3, cmd/2, close/1]).
-export([login/2, dbstats/1, get/4, get/5]).
-include_lib("kernel/include/logger.hrl").

% --- basic interface

connect() -> connect("api.vndb.org", 19535, true).
connect(Host, Port, SSL) ->
	true = jsx:maps_support(),
	Opts = [binary, {packet, 0}, {active, false}],
	TCP = case SSL of false -> gen_tcp; true -> ssl end,
	{ok, Sock} = TCP:connect(Host, Port, Opts, 5000),
	{vndb, Sock, TCP}.

cmd({vndb, S, TCP}, Cmd) ->
	?LOG_DEBUG("cmd: ~s~n", [iolist_to_binary(Cmd)]),
	ok = TCP:send(S, [Cmd, <<4>>]),
	{ok, R} = TCP:recv(S, 0, 5000),

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
			{binary_to_atom(Name, utf8), cmd_recvmore(S, TCP, F)};
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

cmd_recvmore(S, TCP, F) ->
	{ok, Response} = TCP:recv(S, 0, 5000),
	case cmd_term(Response) of
		noterm ->
			{incomplete, G} = F(Response),
			cmd_recvmore(S, TCP, G);
		Term ->
			<<Rest:Term/binary, 4>> = Response,
			{incomplete, G} = F(Rest),
			G(end_stream)
	end.

close({vndb, S, TCP}) -> ok = TCP:close(S).

% --- wrapped commands

login(V, Args) -> vndb:cmd(V, [<<"login ">>, jsx:encode(Args)]).
dbstats(V) -> vndb:cmd(V, <<"dbstats">>).
get(V, Type, Flags, Filters) -> get(V, Type, Flags, Filters, #{}).
get(V, Type, Flags, Filters, Options) ->
	vndb:cmd(V, [
		<<"get ">>,
		atom_to_binary(Type, utf8), <<" ">>,
		lists:join(<<",">>, lists:map(fun(X) -> atom_to_binary(X, utf8) end, Flags)), <<" ">>,
		Filters, <<" ">>,
		jsx:encode(Options)
	]).
