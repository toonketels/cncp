%%%
%%% Utility functions to distribute work across
%%% concurrent processes.
%%%


-module(cncp).

% @TODO: make conditionally or move tests
-include_lib("eunit/include/eunit.hrl").

-export([map/2]).

% Generates a new list by applying the function on each value
% of the list
map(Fun, List) ->
	map(Fun, lists:reverse(List), []).

map(_Fun, [], Acc) ->
	Acc;
map(Fun, [H|T], Acc) ->
	map(Fun, T, [Fun(H)|Acc]).

map_test_() ->
	Square = fun(X) -> X * X end,
    [?_assertEqual([1]        , cncp:map(Square, [1])),
     ?_assertEqual([4]        , cncp:map(Square, [2])),
     ?_assertEqual([1, 4]     , cncp:map(Square, [1, 2])),
     ?_assertEqual([4,9,16,25], cncp:map(Square, [2,3,4,5])),
     ?_assertEqual([225,2809] , cncp:map(Square, [15,53])),
     ?_assertEqual([]         , cncp:map(Square, []))].
