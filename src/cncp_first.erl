%%%
%%% Exexute a list of tasks in concurrently, returning the
%%% result of the first task that is done.
%%%

-module(cncp_first).

-export([first/1, first/2]).


first(List) ->
	first(List, []).

first([], _Args) ->
	no_tasks_to_perform;
first(List, Args) ->
	OpRef = make_ref(),
	first(List, OpRef, Args).

first([], OpRef, _Args) ->
	receive
		{OpRef, Return} -> Return
	end;
first([H|T], OpRef, Args) ->
	WrapperFun = create_wrapper_fun(OpRef, H, Args),
	spawn(WrapperFun),
	first(T, OpRef, Args).

create_wrapper_fun(OpRef, Fun, Args) ->
	Parent = self(),
	WrapperFun = fun() ->
		Return = apply(Fun, Args),
		Parent ! {OpRef, Return}
	end,
	WrapperFun.