%%%-------------------------------------------------------------------
%%% @author  <Nikita Vorontsov>
%%% @copyright (C) 2016, 
%%% @doc
%%% Сервер и клиентская функция для распределенного 
%%% вычисления факториала.
%%% @end
%%% Created :  3 Feb 2016 by  <vorontsov.nstu@yandex.ru>
%%%-------------------------------------------------------------------
-module(novice).

-behaviour(gen_server).

%% API
-export([start/2, start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, product/4, calc/1, get_cores/0]).

-import(lists, [map/2, foldl/3, seq/2]).


-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

start(_Mode, Args) ->
    start_link(Args).

start_link(Args) ->    
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

product(Min, Max, Step, Receivers_pid)->
    gen_server:call(?MODULE, {product, Min, Max, Step, Receivers_pid}).

get_cores()->
    gen_server:call(?MODULE, get_cores).

%% Вычисляет N!
calc(N) when (N =< 1) -> 1;
calc(N) when is_integer(N) ->
    Nodes_cores = foldl(fun (Node, Acc)-> 
				Acc ++ lists:duplicate(rpc:call(Node, novice, get_cores, []), Node)
			end, [], nodes()),
    Total_cores = length(Nodes_cores),
    {First_pid, _} = foldl(
		       fun (Nodes_core, {Receivers_pid, Num})->
			       Next_pid = case rpc:call(Nodes_core, novice, product, [Num, N, Total_cores, Receivers_pid]) of
					      {badrpc, Reason} -> erlang:error(Reason);
					      Pid -> Pid
					      end,
			       {Next_pid, Num+1}
		       end, {self(), 1}, Nodes_cores),
    First_pid ! 1,    
    receive Num -> Num end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Nodes) -> 
    rpc:eval_everywhere(Nodes, gen_server, start_link, [{local, ?SERVER}, ?MODULE, [], []]),
    {ok, normal}.


handle_call({product, Min, Max, Step, Receivers_pid}, _From, State) ->
    Pid= spawn_link(fun()->
			    Fact1 = internal_product(Min, Max, Step),
			    Fact2 = receive Num->Num end,
			    Receivers_pid ! Fact1 * Fact2
		    end),
    {reply, Pid, State};
handle_call(get_cores, _From, State) ->
    {reply, erlang:system_info(logical_processors_available), State}.

handle_info(_Info, State) ->
    {noreply, State}.


terminate(_Reason, _State) ->
    ok.


handle_cast(_Msg, State)->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% Перемножает члены прореженного ряда
internal_product(Min, Max, Step) -> internal_product(Min, Max, Step, 1).
internal_product(Min, Max, _Step, Acc) when Min>Max -> Acc;
internal_product(Min, Max, Step, Acc)-> internal_product(Min+Step, Max, Step, Acc*Min).


