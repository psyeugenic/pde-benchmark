%%
%% Copyright (C) 2013 Björn-Egil Dahlberg
%%
%% File:    pde_benchmark.erl
%% Author:  Björn-Egil Dahlberg
%% Created: 2013-11-20
%%

-module(pde_benchmark).

-define(ets_key(K), K).
-define(ets_val(V), {V,"aljgklfjglkjdlksjglksdjglkjsdglkjsdlgjlskjglkfdsjglsdfjgfdigjfgjfdsigjlsdfigjfdsilgfldisgjsdilgjsf"}).
-define(ets_upd(V), {((element(1,V) * (element(1,V) + 3) * 7) rem (1 bsl 32)),element(2,V)}).
-define(ets_batch_work, 2).

-export([
	main/1
    ]).

main(_) ->
    Dl  = 50,
    N   = 5000,
    Ds  = data(N),
    Tid = setup_ets(?MODULE,[]),
    ok  = populate_ets(Tid, Ds),
    Ks  = keys(Ds),
    ok  = init(Tid,Ks,Dl),
    ok.

init(Tid,Ks,Delay) ->
    pde_benchmark_speedometer:start([self(),2000,0.65]),
    start_creation_timer(self(),300),
    loop(Tid,Ks,Delay,1).

loop(Tid,Ks,Delay,Speed) ->
    receive
	{set_speed, Speed1} ->
	    loop(Tid,Ks,Delay,Speed1);
	create_workers ->
	    create_workers(Tid,Ks,Delay,Speed),
	    loop(Tid,Ks,Delay,Speed)
    end.

create_workers(_Tid,_Ks,_Delay,0) -> [];
create_workers(Tid,Ks,Delay,N) -> 
    Me = self(),
    SKs = shuffle(Ks),
    [spawn_link(fun() -> worker(Me,Tid,SKs,Delay) end)|
	create_workers(Tid,Ks,Delay,N-1)].

worker(Pid,Tid,Ks,T) ->
    worker(Pid,Tid,Ks,T,?ets_batch_work).

worker(Pid,_,[],_,_) -> Pid ! {self(), ok};
worker(Pid,Tid,Ks,T,0) ->
    receive after T -> ok end,
    worker(Pid,Tid,Ks,T);
worker(Pid,Tid,[K|Ks],T,N) ->
    ok = crud(Tid,K),
    worker(Pid,Tid,Ks,T,N - 1).

crud(Tid,K) ->
    [{K,V}] = ets:lookup(Tid,K),
    true = ets:insert(Tid, {K,?ets_upd(V)}),
    ok.

%% aux
start_creation_timer(Pid,Delay) ->
    spawn_link(fun() -> creation_timer_loop(Pid,Delay) end).

creation_timer_loop(Pid,Delay) ->
    receive after Delay ->
	    Pid ! create_workers,
	    creation_timer_loop(Pid,Delay)
    end.

setup_ets(Name,_Opts) ->
    ets:new(Name, [public, named_table, set]).


populate_ets(Tid, Data) ->
    true = ets:insert_new(Tid, Data),
    ok.

data(0) -> [];
data(N) when N > 0 ->
    [{?ets_key(N), ?ets_val(N)}|data(N - 1)].

keys([]) -> [];
keys([{K,_}|Ds]) -> [K|keys(Ds)].

shuffle(Ls) when is_list(Ls) ->
    [I || {_,I} <- lists:sort([{random:uniform(),E} || E <- Ls])].
