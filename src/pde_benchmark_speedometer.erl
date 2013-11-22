%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    pde_benchmark_speedometer.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-11-21
%%

-module(pde_benchmark_speedometer).

-export([
	sigmoid/1,
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
	au,
	us,
	su
    }).

start(Args) ->
    spawn_link(fun() -> init(Args) end).

init([Parent,Delay,Target]) ->
    _  = erlang:system_flag(scheduler_wall_time, true),
    Me = self(),
    spawn_link(fun() -> pressure_timer_loop(Me,60000) end),
    loop(#s{
	    target = Target,
	    pressure = 100,
	    delay = Delay,
	    parent = Parent,
	    speed = 0,
	    logs = openlog(),
	    pts = {os:timestamp(), 0},
	    t0 = os:timestamp(),
	    au = 0,
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

pressure_timer_loop(Pid,Delay) ->
    receive after Delay ->
	    Pid ! calibreate_pressure,
	    pressure_timer_loop(Pid,Delay)
    end.
	    

handle_msg(calibrate_pressure,#s{ au = AvgU, pts = {T0, U0}} = S) ->
    S;
handle_msg(_,S) -> S.

handle_tmo(#s{ t0 = T0, logs=Logs, parent = P, su = Su0, us = Us0 } = S0) ->
    Su1   = lists:sort(erlang:statistics(scheduler_wall_time)),
    U     = utilization(Su1, Su0),
    Us1   = shift_list(U,Us0),
    Util  = lists:sum(Us1)/length(Us1),
    S1    = calibrate_pressure(Util,S0),
    S2    = #s{ speed = N } = update_creation_speed(Util, S1),
    ok    = log(Util, N, T0),
    _     = P ! {set_speed, N},
    datalog(Util,Logs),
    S2#s{ speed = N, au = Util, su = Su1, us = Us1 }.

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

update_creation_speed(Util,S) ->
    N = creation_speed(Util,S),
    S#s{ speed = N }.

creation_speed(Util,#s{pressure=P,target = T}) when Util < T  ->
    Fact = T - Util,
    trunc(P*Fact);
creation_speed(_,_) -> 0.


sigmoid(V) -> V/math:sqrt(1 + V*V).

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
