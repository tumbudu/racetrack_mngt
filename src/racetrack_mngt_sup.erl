%%%-------------------------------------------------------------------
%% @doc racetrack_mngt top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(racetrack_mngt_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(D(X), io_lib:format("~p~n", [X])).


start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
	SupFlags = #{
		strategy => one_for_all,
		intensity => 0,
		period => 1
	},

	ChildSpecs = [
		#{
			id => track_sup,
			start => {geek_track_sup, start_link, []},
			type => supervisor
		}
	],
	{ok, {SupFlags, ChildSpecs}}.

%% internal functions
