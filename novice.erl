-module(novice).
-export([start/2, calc/1]).
-import(lists, [map/2, seq/2, seq/3, foldl/3]).

start(_Mode, Nodes) when is_list(Nodes)->
    Pids = map(fun (Node)-> spawn_link(Node, fun()-> loop() end) end, Nodes),
    put(nodes, Pids),
    {ok, self()}.


rpc(Q, To)->
    To ! {self(), Q},
    receive
	{To, Repl} -> Repl
    end.


loop()->
    receive
	{From, {calc, Arg, Offset, Total_cores, Receivers_pid}} -> %@see calc
	    First_pid = distribute(Arg, Offset, Total_cores, Receivers_pid, my_cores()),
	    From ! {self(), First_pid},
	    loop();
	{From, get_cores} ->			%@see get_cores
	    From ! {self(), my_cores()},
	    loop();
	Else->					%для отладки
	    io:write(error_msg),
	    io:write(Else)
	end.

%% кол-во ядер на машине
get_cores(To)->
    rpc(get_cores, To).

%% сокращение
my_cores()->
    erlang:system_info(logical_processors_available).


%% Вычисляет N!
calc(N) when (N =< 1) -> 1;
calc(N)->
    Total_cores = foldl(fun(Node, Acc)-> 
				get_cores(Node) + Acc 
			end, 0, get(nodes)),
    {_,First_pid} = foldl(fun(Node, {Offset, Receivers_pid})-> % пройдемся по каждой ноде
				  F_pid = rpc({calc,N, Offset, Total_cores, Receivers_pid}, Node), % распределим нагрузку на каждое ядро
				  {Offset+get_cores(Node), F_pid} % новый аккумулятор (другой ряд, другой приемник)
			  end, {1, self()}, get(nodes)), % приемник для последней ноды - потребитель результата
    First_pid ! 1,					 % Первому узлу никто не шлет множитель - пошлём единицу
    receive Num -> Num end.				 % Сообщение от последнего вычислительного узла


%% Распределяет вычисления между ядрами по следующей схеме:
%% (distribute) ------------------------------------
%%     ^     |          |       ...        |       |
%%     |     V          V                  V       |
%%     --(Core 1)<-*-(Core 2)<-*-...<-*-(Core N)<-*1  
distribute(_N, _X, _Cores, Pid, 0)  -> Pid;
distribute(N,X,Cores,Pid1, Counter) -> 
    Pid2 = distribute(N, X+1, Cores, Pid1, Counter-1),
    spawn_link(fun()-> mul_decimated_seq(X,N,Cores,Pid2) end).
    
%% Порождает прореженный количеством ядер натуральный ряд, перемножает его члены,
%% домножает его на присланный множитель и возвращает результат по указанному Pid
%% [  c  ] <--1    [  c  ]  Пример балансировки нагрузки прореживанием для двух ядер
%% [  o  ] *  2--> [  o  ]
%% [  r  ] <--3  * [  r  ]
%% [  e  ] *  4--> [  e  ]
%% [  1  ] <--5    [  2  ]
mul_decimated_seq(X,N,Cores,Pid)->
    List = seq(X,N,Cores),
    Multiplier1 = foldl(fun(A, Acc) -> A * Acc end, 1, List),
    Multiplier2 = receive Num -> Num end,
    Pid ! Multiplier1 * Multiplier2.
