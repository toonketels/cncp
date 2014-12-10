%%%
%%% Accepts a list of functions and executes them concurrently,
%%% collecting the results.
%%%

-module(parallel).

-include_lib("eunit/include/eunit.hrl").

-export([parallel/1, parallel/2, 
	     cparallel/1, cparallel/2, 
	     cparallel_limit/2, cparallel_limit/3]).

-export([parallel_t/0, cparallel_t/0, cparallel_limit_t/0]).

parallel(FunList) ->
	parallel(FunList, []).

parallel(FunList, Args) ->
	WrapperFun = fun(Fun) ->
		apply(Fun, Args)
	end,
	cncp:map(WrapperFun, FunList).


cparallel(FunList) ->
	cparallel(FunList, []).

cparallel(FunList, Args) ->	
	WrapperFun = fun(Fun) ->
		apply(Fun, Args)
	end,
	cncp:cmap(WrapperFun, FunList).


cparallel_limit(FunList, Limit) ->
	cparallel_limit(FunList, [], Limit).

cparallel_limit(FunList, Args, Limit) ->	
	WrapperFun = fun(Fun) ->
		apply(Fun, Args)
	end,
	cncp:cmap_limit(WrapperFun, FunList, Limit).



parallel_t() ->
	One   = fun(X) -> 
		io:format("one"),
		timer:sleep(1800),
		1 * X 
	end,
	Two   = fun(X) -> 
		io:format("two"),
		timer:sleep(1000),
		2 * X 
	end,
	Three = fun(X) ->
		io:format("three"),
		timer:sleep(1500),
		3 * X 
	end,
	parallel([One, Two, Three], [5]).

cparallel_t() ->
	One   = fun(X) -> 
		io:format("one"),
		timer:sleep(1800),
		1 * X 
	end,
	Two   = fun(X) -> 
		io:format("two"),
		timer:sleep(1000),
		2 * X 
	end,
	Three = fun(X) ->
		io:format("three"),
		timer:sleep(1500),
		3 * X 
	end,
	cparallel([One, Two, Three], [5]).


cparallel_limit_t() ->
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
	parallel([One, Two, Three, Four, Five, Six], [5]),
	
	io:format("~n~n parallel ~n~n"),
	cparallel([One, Two, Three, Four, Five, Six], [5]),

	io:format("~n~n limit ~n~n"),
	cparallel_limit([One, Two, Three, Four, Five, Six], [5], 3).