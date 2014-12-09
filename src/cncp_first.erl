%%%
%%% Execute a list of tasks in concurrently, returning the
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
	first(List, OpRef, Args, []).

first([], OpRef, _Args, Refs) ->
	receive
		{OpRef, Return} -> 
			lists:foreach(
				fun(Ref) -> demonitor(Ref, [flush]) end,
				Refs
			),
			Return
	end;
first([H|T], OpRef, Args, Refs) ->
	WrapperFun = create_wrapper_fun(OpRef, H, Args),
	{_Pid, Ref} = spawn_monitor(WrapperFun),
	first(T, OpRef, Args, [Ref|Refs]).

create_wrapper_fun(OpRef, {Mod, Fun}, Args) ->
	Parent = self(),
	WrapperFun = fun() ->
		Return = apply(Mod, Fun, Args),
		Parent ! {OpRef, Return}
	end,
	WrapperFun;
create_wrapper_fun(OpRef, Fun, Args) ->
	Parent = self(),
	WrapperFun = fun() ->
		Return = apply(Fun, Args),
		Parent ! {OpRef, Return}
	end,
	WrapperFun.