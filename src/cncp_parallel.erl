%%%
%%% Accepts a list of functions and executes them concurrently,
%%% collecting the results.
%%%

-module(cncp_parallel).

-include_lib("eunit/include/eunit.hrl").

-export([parallel/1, parallel/2, 
	     cparallel/1, cparallel/2, 
	     cparallel_limit/2, cparallel_limit/3]).

-export([t/0, one/1, two/1, three/1, four/1, five/1, parallel_t/0]).

parallel(FunList) ->
	parallel(FunList, []).

parallel(FunList, Args) ->
	WrapperFun = fun
		({Mod, Fun}) -> apply(Mod, Fun, Args);
		(Fun) -> apply(Fun, Args)
	end,
	cncp:foreach(WrapperFun, FunList).


cparallel(FunList) ->
	cparallel(FunList, []).

cparallel(FunList, Args) ->	
	WrapperFun = fun
		({Mod, Fun}) -> apply(Mod, Fun, Args);
		(Fun) -> apply(Fun, Args)
	end,
	cncp:cforeach(WrapperFun, FunList).


cparallel_limit(FunList, Limit) ->
	cparallel_limit(FunList, [], Limit).

cparallel_limit(FunList, Args, Limit) ->	
	WrapperFun = fun
		({Mod, Fun}) -> apply(Mod, Fun, Args);
		(Fun) -> apply(Fun, Args)
	end,
	cncp:cforeach_limit(WrapperFun, FunList, Limit).

one(X) ->
	io:format("~p one start ~n", [X]),
	timer:sleep(1000),
	io:format("~p one stop ~n", [X]).

two(X) ->
	io:format("~p two start ~n", [X]),
	timer:sleep(1000),
	io:format("~p two stop ~n", [X]).

three(X) ->
	io:format("~p three start ~n", [X]),
	timer:sleep(1000),
	io:format("~p three stop ~n", [X]).

four(X) ->
	io:format("~p four start ~n", [X]),
	timer:sleep(1000),
	io:format("~p four stop ~n", [X]).

five(X) ->
	io:format("~p five start ~n", [X]),
	timer:sleep(1000),
	io:format("~p five stop ~n", [X]).



parallel_t() ->

	L = [{cncp_parallel, one},
	     {cncp_parallel, two},
	     {cncp_parallel, three},
	     {cncp_parallel, four},
	     {cncp_parallel, five}],

	cncp_parallel:cparallel_limit(L, [2], 2).



t() ->
	Maker = fun(Name, Times, Timeout) ->
		fun(X) ->
			io:format("~p ~p ~n", [Name, Times]),
			timer:sleep(Timeout),
			io:format("~p ~p done~n", [Name, Times]),
			X * Times
		end
	end,

	One   = Maker(one, 1, 1800),
	Two   = Maker(two, 2, 1000),
	Three = Maker(three, 3, 1300), 
	Four  = Maker(four, 4, 1800), 
	Five  = Maker(five, 5, 1200), 
	Six = Maker(three, 6, 900), 

	io:format("~n~n synchronous ~n~n"),
	parallel([One, Two, Three, Four, Five, Six], [5]),
	
	io:format("~n~n parallel ~n~n"),
	cparallel([One, Two, Three, Four, Five, Six], [5]),

	io:format("~n~n limit ~n~n"),
	cparallel_limit([One, Two, Three, Four, Five, Six], [5], 2).