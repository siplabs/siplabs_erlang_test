-module(novice).
-export([calc/1]).
-import(lists, [map/2, seq/2, foldl/3]).


%% Вычисляет N!
calc(N) when (N =< 1) -> 1;
calc(N) when is_integer(N) ->
    Cores = erlang:system_info(logical_processors_available),
    Core_numbers = seq(1, Cores),
    First_pid = foldl(
		  fun (Core_number, Receivers_pid)->
			  spawn_link(fun()-> 
					     Fact1 = product(Core_number, N, Cores),
					     Fact2 = receive Num->Num end,
					     Receivers_pid ! Fact1 * Fact2
				     end)
		  end, self(), Core_numbers),
    First_pid ! 1,
    receive Num -> Num end.

%% Перемножает члены прореженного ряда
product(Min, Max, Step) -> product(Min, Max, Step, 1).
product(Min, Max, _Step, Acc) when Min>Max -> Acc;
product(Min, Max, Step, Acc)-> product(Min+Step, Max, Step, Acc*Min).
