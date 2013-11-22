%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    pde_benchmark_log.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-11-21
%%

-module(pde_benchmark_log).

-record(log, {
	t0,
	fd
    }).

-export([
	open/1,
	close/1,
	write/2
    ]).


open(Filename) ->
    {ok, Fd} = file:open(Filename, [write]),
    #log{
	t0 = os:timestamp(),
	fd = Fd
    }.

close(#log{fd =Fd}) ->
    file:close(Fd),
    ok.

write(#log{ fd = Fd, t0 = T0}, Val) ->
    Df = timer:now_diff(os:timestamp(),T0) div 1000000,
    write(Fd, Df, Val),
    ok.

write(Fd, Df, Val) when is_integer(Val) ->
    io:format(Fd, "~w\t~w~n", [Df, Val]),
    ok;
write(Fd, Df, Val) when is_float(Val) ->
    io:format(Fd, "~w\t~.4f~n", [Df, Val]),
    ok.
