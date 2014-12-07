%%%
%%% Utility functions to distribute work across
%%% concurrent processes.
%%%

-module(cncp).

% @TODO: make conditionally or move tests
-include_lib("eunit/include/eunit.hrl").

-export([map/2, cmap/2, cmap_limit/3]).


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