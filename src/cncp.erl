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
	cmap_limit_collect_go(Fun, List, Limit, Running, Spawned, Returned, OpRef);
% We  have reached the limit
cmap_limit_spawn(Fun, [H|T], Limit, Limit, Spawned, Returned, OpRef) ->
	% Spawn, wait on receive before spawning again
	% Prepare function to spawn
	{Ref, WrapperFun} = cmap_limit_create_wrapper_fn(OpRef, Fun, H),
	spawn(WrapperFun),
	cmap_limit_collect_go(Fun, T, Limit, Limit+1, [Ref|Spawned], Returned, OpRef);
% We have not yet reached the limit, keep on spawning
cmap_limit_spawn(Fun, [H|T], Limit, Running, Spawned, Returned, OpRef) ->
	% Spawn, wait on receive before spawning again
	% Prepare function to spawn
	{Ref, WrapperFun} = cmap_limit_create_wrapper_fn(OpRef, Fun, H),
	spawn(WrapperFun),
	cmap_limit_spawn(Fun, T, Limit, Running+1, [Ref|Spawned], Returned, OpRef).

% There are no more processes to spawn, and only one (the last) process is running,
% when it returns, we're done.
cmap_limit_collect_go(_Fun, [], _Limit, 2, Spawned, Returned, OpRef) ->
	case cmap_limit_collect(OpRef, Returned) of
		{error, Reason} -> {error, Reason};
		Returned2       -> cmap_limit_assemble_results(Spawned, Returned2)
	end;
% There are no processes to spawn, but there are still some processes running,
% keep waiting for each of them to finish.
cmap_limit_collect_go(Fun, [] = List, Limit, Running, Spawned, Returned, OpRef) ->
	case cmap_limit_collect(OpRef, Returned) of
		{error, Reason} -> {error, Reason};
		Returned2       -> cmap_limit_collect_go(Fun, List, Limit, Running - 1, Spawned, Returned2, OpRef)
	end;
% There are still processes to spawn, so spawn as soon as we hit our value
cmap_limit_collect_go(Fun, List, Limit, Running, Spawned, Returned, OpRef) ->
	case cmap_limit_collect(OpRef, Returned) of
		{error, Reason} -> {error, Reason};
		Returned2       -> cmap_limit_spawn(Fun, List, Limit, Running - 1, Spawned, Returned2, OpRef)
	end.
cmap_limit_assemble_results(Spawned, Returned) ->
	ReturnedOrdered = lists:map(fun(Ref) ->
		{ok, Value} = dict:find(Ref, Returned),
		Value
	end, Spawned),
	lists:reverse(ReturnedOrdered).

% Creates a wrapper fn to be spawned around the passed
% Fn and arg. We do so to hook it up to communicate back
% to us when the fun has returned.
cmap_limit_create_wrapper_fn(OpRef, Fun, Arg) ->
	Parent = self(),
	Ref    = make_ref(),
	WrapperFun = fun() ->
		Return = Fun(Arg),
		Parent ! {OpRef, Ref, Return}
	end,
	{Ref, WrapperFun}.

% Collects a return value of the spawned process function
% to the collection of return values.
cmap_limit_collect(OpRef, Returned) ->
	receive
		{OpRef, Ref, Return} ->
			dict:store(Ref, Return, Returned)
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
