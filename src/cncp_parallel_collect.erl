%%%
%%% Accepts a list of functions and executes them concurrently,
%%% collecting the results.
%%%

-module(cncp_parallel_collect).

-include_lib("eunit/include/eunit.hrl").

-export([parallel_collect/1, parallel_collect/2, 
	     cparallel_collect/1, cparallel_collect/2, 
	     cparallel_collect_limit/2, cparallel_collect_limit/3]).

-export([parallel_t/0]).

parallel_collect(FunList) ->
	parallel_collect(FunList, []).

parallel_collect(FunList, Args) ->
	WrapperFun = fun(Fun) ->
		apply(Fun, Args)
	end,
	cncp:map(WrapperFun, FunList).


cparallel_collect(FunList) ->
	cparallel_collect(FunList, []).

cparallel_collect(FunList, Args) ->	
	WrapperFun = fun(Fun) ->
		apply(Fun, Args)
	end,
	cncp:cmap(WrapperFun, FunList).


cparallel_collect_limit(FunList, Limit) ->
	cparallel_collect_limit(FunList, [], Limit).

cparallel_collect_limit(FunList, Args, Limit) ->	
	WrapperFun = fun(Fun) ->
		apply(Fun, Args)
	end,
	cncp:cmap_limit(WrapperFun, FunList, Limit).



parallel_t() ->
	Maker = fun(Name, Times, Timeout) ->
		fun(X) ->
			io:format("~p ~p ~n", [Name, Times]),
			timer:sleep(Timeout),
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
	parallel_collect([One, Two, Three, Four, Five, Six], [5]),
	
	io:format("~n~n parallel ~n~n"),
	cparallel_collect([One, Two, Three, Four, Five, Six], [5]),

	io:format("~n~n limit ~n~n"),
	cparallel_collect_limit([One, Two, Three, Four, Five, Six], [5], 3).