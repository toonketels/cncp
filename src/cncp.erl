%%%
%%% Utility functions to distribute work across
%%% concurrent processes.
%%%

-module(cncp).

% @TODO: make conditionally or move tests
-include_lib("eunit/include/eunit.hrl").

-export([map/2, cmap/2, cmap_limit/3, 
         foreach/2, cforeach/2, cforeach_limit/3, 
         first/1, first/2,
         parallel/1, parallel/2, cparallel/1, cparallel/2, cparallel_limit/2, cparallel_limit/3]).

-export([parallel_t/0]).


% Generates a new list by applying the function on each value
% of the list
map(Fun, List) ->
	cncp_map:map(Fun, List).


% Generates a new list by spawning a function in a new process
% and capturing its return value.
cmap(Fun, List) ->
	cncp_map:cmap(Fun, List).
	

% Generates a new list by spawning a function in a new process
% and capturing its return value. Unlike cmap, cmap_limit ensures
% no more then `limit` processes are running at a time.
% Use this to limit the number of concurrent processes to not
% overload some system.
cmap_limit(Fun, List, Limit) ->
	cncp_map:cmap_limit(Fun, List, Limit).

foreach(Fun, List) ->
    cncp_foreach:foreach(Fun, List).

cforeach(Fun, List) ->
    cncp_foreach:cforeach(Fun, List).

cforeach_limit(Fun, List, Limit) ->
    cncp_foreach:cforeach_limit(Fun, List, Limit).


first(FunList) ->
    cncp_first:first(FunList).

first(FunList, Args) ->
    cncp_first:first(FunList, Args).


parallel(FunList) ->
    cncp_parallel:parallel(FunList).

parallel(FunList, Args) ->
    cncp_parallel:parallel(FunList, Args).

cparallel(FunList) ->
    cncp_parallel:cparallel(FunList).

cparallel(FunList, Args) ->
    cncp_parallel:cparallel(FunList, Args).

cparallel_limit(FunList, Limit) ->
    cncp_parallel:cparallel_limit(FunList, Limit).

cparallel_limit(FunList, Args, Limit) ->
    cncp_parallel:cparallel_limit(FunList, Args, Limit).



map_test_() ->
	Square = fun(X) -> X * X end,
    [?_assertEqual([1]        , cncp:map(Square, [1])),
     ?_assertEqual([4]        , cncp:map(Square, [2])),
     ?_assertEqual([1, 4]     , cncp:map(Square, [1, 2])),
     ?_assertEqual([4,9,16,25], cncp:map(Square, [2,3,4,5])),
     ?_assertEqual([225,2809] , cncp:map(Square, [15,53])),
     ?_assertEqual([]         , cncp:map(Square, []))].

cmap_test_() ->
	Square = fun(X) -> X * X end,
    [?_assertEqual([1]        , cncp:cmap(Square, [1])),
     ?_assertEqual([4]        , cncp:cmap(Square, [2])),
     ?_assertEqual([1, 4]     , cncp:cmap(Square, [1, 2])),
     ?_assertEqual([4,9,16,25], cncp:cmap(Square, [2,3,4,5])),
     ?_assertEqual([225,2809] , cncp:cmap(Square, [15,53])),
     ?_assertEqual([]         , cncp:cmap(Square, []))].

cmap_limit_test_() ->
	Square = fun(X) -> X * X end,
    [?_assertEqual([4]        , cncp:cmap_limit(Square, [2], 0)),
     ?_assertEqual([4]        , cncp:cmap_limit(Square, [2], 1)),
     ?_assertEqual([4]        , cncp:cmap_limit(Square, [2], 5)),
     ?_assertEqual([4,9,16,25], cncp:cmap_limit(Square, [2,3,4,5], 0)),
     ?_assertEqual([4,9,16,25], cncp:cmap_limit(Square, [2,3,4,5], 1)),
     ?_assertEqual([4,9,16,25], cncp:cmap_limit(Square, [2,3,4,5], 2)),
     ?_assertEqual([4,9,16,25], cncp:cmap_limit(Square, [2,3,4,5], 4)),
     ?_assertEqual([4,9,16,25], cncp:cmap_limit(Square, [2,3,4,5], 40)),
     ?_assertEqual([]         , cncp:cmap_limit(Square, [], 0)),
     ?_assertEqual([]         , cncp:cmap_limit(Square, [], 1)),
     ?_assertEqual([]         , cncp:cmap_limit(Square, [], 10))].


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
    cncp:parallel([One, Two, Three, Four, Five, Six], [5]),
    
    io:format("~n~n parallel ~n~n"),
    cncp:cparallel([One, Two, Three, Four, Five, Six], [5]),

    io:format("~n~n limit ~n~n"),
    cncp:cparallel_limit([One, Two, Three, Four, Five, Six], [5], 3).