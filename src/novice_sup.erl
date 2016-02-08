%%%-------------------------------------------------------------------
%%% @author  <ne@ne>
%%% @copyright (C) 2016, 
%%% @doc
%%%
%%% @end
%%% Created :  5 Feb 2016 by  <ne@ne>
%%%-------------------------------------------------------------------
-module(novice_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(Args) ->
    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    AChild = #{id => 'novice',
	       start => {'novice', start_link, [Args]},
	       restart => permanent,
	       shutdown => 5000,
	       type => worker,
	       modules => ['novice']},

    {ok, {SupFlags, [AChild]}}.


%%%===================================================================
%%% Internal functions
%%%===================================================================
