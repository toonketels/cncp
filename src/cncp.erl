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
	map(Fun, List, []).

map(_Fun, [], Acc) ->
	lists:reverse(Acc);
map(Fun, [H|T], Acc) ->
	map(Fun, T, [Fun(H)|Acc]).


% Generates a new list by spawning a function in a new process
% and capturing its return value.
cmap(Fun, List) ->
	% For each process, spawn it
	% Assemble a list of expected return messages
	% Receive one message at a time
	cmap_spawn(Fun, List, []).

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
	lists:reverse(Acc);
cmap_receive([{Child, Ref}|T], Acc) ->
	receive
		{Child, Ref, Return} ->
			cmap_receive(T, [Return|Acc])
	% @TODO: make it configurable or so
	after 2000 ->
		{error, timeout}
	end.
	

% Generates a new list by spawning a function in a new process
% and capturing its return value. Unlike cmap, cmap_limit ensures
% no more then `limit` processes are running at a time.
% Use this to limit the number of concurrent processes to not
% overload some system.
cmap_limit(_Fun, [], _Limit) ->
	[];
cmap_limit(Fun, List, Limit) ->
	cmap_limit_spawn(Fun, List, Limit, 1, [], dict:new(), make_ref()).

% No more processes to spawn
cmap_limit_spawn(Fun, [] = List, Limit, Running, Spawned, Returned, OpRef) ->
	cmap_limit_receive_spawn(Fun, List, Limit, Running, Spawned, Returned, OpRef);
% We  have reached the limit
cmap_limit_spawn(Fun, [H|T], Limit, Limit, Spawned, Returned, OpRef) ->
	% Spawn, wait on receive before spawning again
	% Prepare function to spawn
	Parent = self(),
	Ref    = make_ref(),
	WrapperFun = fun() ->
		io:format("in another proc ~p~n", [self()]),
		io:format("Arg passed ~p ~n", [H]),
		Return = Fun(H),
		Parent ! {OpRef, Ref, Return}
	end,
	spawn(WrapperFun),
	cmap_limit_receive_spawn(Fun, T, Limit, Limit+1, [Ref|Spawned], Returned, OpRef);
% We have not yet reached the limit, keep on spawning
cmap_limit_spawn(Fun, [H|T], Limit, Running, Spawned, Returned, OpRef) ->
	% Spawn, wait on receive before spawning again
	% Prepare function to spawn
	Parent = self(),
	Ref    = make_ref(),
	WrapperFun = fun() ->
		io:format("in another proc ~p~n", [self()]),
		io:format("Arg passed ~p ~n", [H]),
		Return = Fun(H),
		Parent ! {OpRef, Ref, Return}
	end,
	spawn(WrapperFun),
	cmap_limit_spawn(Fun, T, Limit, Running+1, [Ref|Spawned], Returned, OpRef).

% There are no more processes to spawn, and only one (the last) process is running
cmap_limit_receive_spawn(_Fun, [], _Limit, 2, Spawned, Returned, OpRef) ->
	receive
		{OpRef, Ref, Return} ->
			io:format("Returned ~p ~n", [Return]),
			Returned2 = dict:store(Ref, Return, Returned),
			cmap_limit_assemble_results(Spawned, Returned2)
	% @TODO: make it configurable or so
	after 2000 ->
		{error, timeout}
	end;
% Thre are no processes to spawn, but there are still some processes running
cmap_limit_receive_spawn(Fun, [] = List, Limit, Running, Spawned, Returned, OpRef) ->
	receive
		{OpRef, Ref, Return} ->
			io:format("Returned ~p ~n", [Return]),
			Returned2 = dict:store(Ref, Return, Returned),
			cmap_limit_receive_spawn(Fun, List, Limit, Running - 1, Spawned, Returned2, OpRef)
	% @TODO: make it configurable or so
	after 2000 ->
		{error, timeout}
	end;
% There are still processes to spawn, but first we need to receive
cmap_limit_receive_spawn(Fun, List, Limit, Running, Spawned, Returned, OpRef) ->
	receive
		{OpRef, Ref, Return} ->
			io:format("Returned ~p ~n", [Return]),
			Returned2 = dict:store(Ref, Return, Returned),
			cmap_limit_spawn(Fun, List, Limit, Running - 1, Spawned, Returned2, OpRef)
	% @TODO: make it configurable or so
	after 2000 ->
		{error, timeout}
	end.
cmap_limit_assemble_results(Spawned, Returned) ->
	ReturnedOrdered = lists:map(fun(Ref) ->
		{ok, Value} = dict:find(Ref, Returned),
		io:format("RETRURNRDERE ~p~n", [Value]),
		Value
	end, Spawned),
	lists:reverse(ReturnedOrdered).



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
