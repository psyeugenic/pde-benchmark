%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    pde_benchmark_speedometer.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-11-21
%%

-module(pde_benchmark_speedometer).

-export([
	start/1
    ]).

-record(s, {
	t0,
	delay,
	pressure,
	parent,
	target,
	speed,
	logs,
	pts,
	us,
	su
    }).

start(Args) ->
    spawn_link(fun() -> init(Args) end).

init([Parent,Delay,Target]) ->
    _ = erlang:system_flag(scheduler_wall_time, true),
    loop(#s{
	    target = Target,
	    pressure = 120,
	    delay = Delay,
	    parent = Parent,
	    speed = 0,
	    logs = openlog(),
	    pts = {os:timestamp(), 0},
	    t0 = os:timestamp(),
	    us = [0,0,0,0,0,0,0,0,0,0,0,0],
	    su = lists:sort(erlang:statistics(scheduler_wall_time))
	}).

loop(#s{delay=D}=S) ->
    receive
	Msg ->
	    loop(handle_msg(Msg, S))
    after D ->
	    loop(handle_tmo(S))
    end.

handle_msg(_,S) -> S.

handle_tmo(#s{ t0 = T0, logs=Logs, parent = P, su = Su0, us = Us0 } = S0) ->
    Su1   = lists:sort(erlang:statistics(scheduler_wall_time)),
    U     = utilization(Su1, Su0),
    Us1   = shift_list(U,Us0),
    Util  = lists:sum(Us1)/length(Us1),
    S1    = calibrate_pressure(Util,S0),
    N     = creation_speed(Util, S1),
    ok    = log(Util, N, T0),
    _     = P ! {set_speed, N},
    datalog(Util,Logs),
    S1#s{ speed = N, su = Su1, us = Us1 }.

%% aux

openlog() -> [
	pde_benchmark_log:open("mem.dat"),
	pde_benchmark_log:open("procs.dat"),
	pde_benchmark_log:open("util.dat")
    ].

closelog(Fds) ->
    [pde_benchmark:close(Fd) || Fd <- Fds],
    ok.

datalog(Util, [Fdm,Fdp,Fdu]) ->
    pde_benchmark_log:write(Fdm, erlang:memory(total)),
    pde_benchmark_log:write(Fdp, erlang:system_info(process_count)),
    pde_benchmark_log:write(Fdu, Util),
    ok.

shift_list(_,[]) -> [];
shift_list(U,[_]) -> [U];
shift_list(U,[_|Us]) ->
    lists:reverse([U|lists:reverse(Us)]).

calibrate_pressure(_U1,S) -> S.
%calibrate_pressure(U1,#s{pts={T0,U0}, target =T, pressure=P }=S) ->
%	T1 = os:timestamp(),
%	case timer:now_diff(T1, T0) div 1000000 of
%	    Df when Df > 60 andalso T - U1 > 0.35 andalso abs(U1-U0) < 0.1 ->
%		S#s{ pts={T1,U1}, pressure = P + 3 };
%	    Df when Df > 60 ->
%		S#s{ pts={T1,U1} };
%	    _ ->
%		S
%	end.

creation_speed(Util,#s{pressure=P,target = T}) when T > Util ->
    Fact = T - Util,
    trunc((Fact * P)) + 1;
creation_speed(_,_) -> 1.

utilization(Ts1,Ts0) ->
    {N, Sum} = lists:foldl(fun
	    ({{_,A1,T1},{_,A0,T0}},{I,S}) ->
		{I + 1, S + (A1 - A0)/(T1 - T0)}
	end, {0,0}, lists:zip(Ts1,Ts0)),
    Sum/N.

log(Util, Speed, T0) ->
    T1 = os:timestamp(),
    Df = timer:now_diff(T1,T0) div 1000000,
    Mi = Df div 60,
    Se = Df rem 60,
    io:format("\r~w: [~.5f]  processes: ~4w (speed ~3w) duration: ~wm~2.2.0ws" , [
	    ?MODULE,
	    Util,
	    erlang:system_info(process_count),
	    Speed,
	    Mi,Se
	]),
    ok.
