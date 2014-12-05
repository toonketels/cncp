%%%
%%% Utility functions to distribute work across
%%% concurrent processes.
%%%


-module(cncp).

% @TODO: make conditionally or move tests
-include_lib("eunit/include/eunit.hrl").

-export([map/2, cmap/2]).


% Generates a new list by applying the function on each value
% of the list
map(Fun, List) ->
	map(Fun, lists:reverse(List), []).

map(_Fun, [], Acc) ->
	Acc;
map(Fun, [H|T], Acc) ->
	map(Fun, T, [Fun(H)|Acc]).


% Generates a new list by spawning a function in a new process
% and capturing its return value.
cmap(Fun, List) ->
	% For each process, spawn it
	% Assemble a list of expected return messages
	% Receive one message at a time
	cmap_spawn(Fun, lists:reverse(List), []).

cmap_spawn(_Fun, [], Spawned) ->
	% Done spawning go into receive mode
	cmap_receive(lists:reverse(Spawned), []);
cmap_spawn(Fun, [H|T], Spawned) ->
	Parent = self(),
	Ref    = make_ref(),
	WrapperFun = fun() ->
		io:format("in another proc ~p~n", [self()]),
		Return = Fun(H),
		Parent ! {self(), Ref, Return}
	end,
	Child = spawn(WrapperFun),
	cmap_spawn(Fun, T, [{Child, Ref}|Spawned]).

cmap_receive([], Acc) ->
	% Nothing more to receive, we're done
	Acc;
cmap_receive([{Child, Ref}|T], Acc) ->
	receive
		{Child, Ref, Return} ->
			cmap_receive(T, [Return|Acc])
	% @TODO: make it configurable or so
	after 2000 ->
		{error, timeout}
	end.


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
     ?_assertEqual([4]        , cncp:map(Square, [2])),
     ?_assertEqual([1, 4]     , cncp:map(Square, [1, 2])),
     ?_assertEqual([4,9,16,25], cncp:map(Square, [2,3,4,5])),
     ?_assertEqual([225,2809] , cncp:map(Square, [15,53])),
     ?_assertEqual([]         , cncp:map(Square, []))].
