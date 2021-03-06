-module(cncp_foreach).

-export([foreach/2, cforeach/2, cforeach_limit/3]).

-export([test_fact_sequential/0, test_fact_concurrent/0, test_fact_limit/0, ddouble/1]).

% @TODO: make conditionally or move tests
-include_lib("eunit/include/eunit.hrl").


ddouble(X) ->
	io:format("~p start ~n", [X]),
	timer:sleep(500),
	io:format("~p stop ~n", [X]).


test_fact_sequential() ->

	% Fun = fun(X) -> io:format("~p~n", [X]), timer:sleep(500) end,

	List = lists:seq(1, 10),
	% foreach(Fun, List).
	foreach({cncp_foreach, ddouble}, List).

test_fact_concurrent() ->

	% Fun = fun(X) -> io:format("~p~n", [X]), timer:sleep(500) end,

	% List = lists:seq(1, 100),
	% cforeach(Fun, List).

	List = lists:seq(1, 100),
	cforeach({cncp_foreach, ddouble}, List).

test_fact_limit() ->

	% Fun = fun(X) -> io:format("~p~n", [X]), timer:sleep(500) end,

	% List = lists:seq(1, 100),
	% cforeach_limit(Fun, List, 10).

	List = lists:seq(1, 100),
	cforeach_limit({cncp_foreach, ddouble}, List, 10).


foreach(_Fun, []) ->
	ok;
foreach({Mod, Fun}, [H|T]) ->
	Mod:Fun(H),
	foreach({Mod, Fun}, T);
foreach(Fun, [H|T]) ->
	Fun(H),
	foreach(Fun, T).

loop(State) ->
	receive
		{From, Ref, result} -> From ! {Ref, lists:reverse(State)};
		M                   -> loop([M|State])
	after 500 ->
		timeout
	end.

cforeach(_Fun, []) ->
	ok;
cforeach({Mod, Fun}, [H|T]) ->
	WrapperFun = fun() ->
		Mod:Fun(H)
	end,
	spawn(WrapperFun),
	cforeach({Mod, Fun}, T);
cforeach(Fun, [H|T]) ->
	WrapperFun = fun() ->
		Fun(H)
	end,
	spawn(WrapperFun),
	cforeach(Fun, T).


cforeach_limit(Fun, List, Limit) when Limit > 0 ->
	cforeach_limit(Fun, List, Limit, 0, [], []).
% List is empty, we're done
cforeach_limit(_Fun, [], _Limit, _Running, Refs, Msgs) ->
	% Demonitor any outstanding refs so the proc mailbox wont get
	% poluted with incoming DOWN messages
	lists:foreach(fun(MonitorRef) -> demonitor(MonitorRef, [flush]) end, Refs),
	% If we intercepted DOWN messages from other proces, put them
	% back into the mailbox in the correct order
	lists:foreach(fun(Msg) -> self() ! Msg end, lists:reverse(Msgs)),
	% Return done
	ok;
% The limit has been reached, wait for a
% proc to terminate before spawning a new one
cforeach_limit(Fun, List, Limit, Limit, Refs, Msgs) ->
	receive
		{'DOWN',Ref,process,_Pid,_Reason} = M ->
			% Check if the DOWN message is of one of our procs
			case lists:member(Ref, Refs) of
				% If so, decrease running proc count and recur
				true -> 
					cforeach_limit(Fun, List, Limit, Limit-1, lists:delete(Ref, Refs), Msgs);
				% If not, this is not one of our monitored procs, so
				% play nice: collect them so we can put them back
				% into the mailbox when we're done
				false ->
					cforeach_limit(Fun, List, Limit, Limit, Refs, [M|Msgs])
			end
	% @todo: make configurable
	after 2000 ->
		{error, timeout}
	end;
% Limit not yet been reached, spawn some more
cforeach_limit({Mod, Fun}, [H|T], Limit, Running, Refs, Msgs) ->
	WrapperFun = fun() ->
		Mod:Fun(H)
	end,
	{_Pid, Ref} = spawn_monitor(WrapperFun),
	cforeach_limit({Mod, Fun}, T, Limit, Running+1, [Ref|Refs], Msgs);
% Limit not yet been reached, spawn some more
cforeach_limit(Fun, [H|T], Limit, Running, Refs, Msgs) ->
	WrapperFun = fun() ->
		Fun(H)
	end,
	{_Pid, Ref} = spawn_monitor(WrapperFun),
	cforeach_limit(Fun, T, Limit, Running+1, [Ref|Refs], Msgs).


foreach_test_() ->
	Server = fun() ->
		loop([])
	end,
	
	Test = fun(List) ->

		S = spawn(Server),
		Send = fun(X) -> S ! X  end,

		ok  = cncp_foreach:foreach(Send, List),
		Ref = make_ref(),
		S ! {self(), Ref, result},
		receive
			{Ref, Result} ->
				io:format("STATE ~p~n", [Result]),
				Result
		after 500 ->
			timeout
		end
	end,

    
    [?assertEqual([1,2,3], Test([1,2,3])),
     ?assertEqual([1,2,3,4], Test([1,2,3,4]))].

cforeach_test_() ->
	Server = fun() ->
		loop([])
	end,
	
	Test = fun(List) ->

		S = spawn(Server),
		Send = fun(X) -> S ! X  end,

		ok  = cncp_foreach:cforeach(Send, List),
		Ref = make_ref(),
		timer:sleep(200),
		S ! {self(), Ref, result},
		receive
			{Ref, Result} ->
				io:format("STATE ~p~n", [Result]),
				% We no longer get the results back in order,
				% so sort before comparing
				lists:sort(Result)
		after 500 ->
			timeout
		end
	end,

    
    [?assertEqual([1,2,3], Test([1,2,3])),
     ?assertEqual([1,2,3,4], Test([1,2,3,4]))].

cforeach_limit_test_() ->
	Server = fun() ->
		loop([])
	end,
	
	Test = fun(List, Limit) ->

		S = spawn(Server),
		Send = fun(X) -> S ! X  end,

		ok  = cncp_foreach:cforeach_limit(Send, List, Limit),
		Ref = make_ref(),
		timer:sleep(200),
		S ! {self(), Ref, result},
		receive
			{Ref, Result} ->
				io:format("STATE ~p~n", [Result]),
				% We no longer get the results back in order,
				% so sort before comparing
				lists:sort(Result)
		after 500 ->
			timeout
		end
	end,

    
    [?assertEqual([1,2,3], Test([1,2,3], 1)),
     ?assertEqual([1,2,3,4], Test([1,2,3,4], 2))].

