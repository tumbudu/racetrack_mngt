-module(geek_track_sup).
-behaviour(supervisor).
-export([start_link/0]).
-export([init/1, get_child_specs/0]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	SupFlags = #{
		strategy => one_for_one,
		% intensity => 0, % default 5
		period => 1
	},
	ChildSpecs = get_child_specs(),
	{ok, {SupFlags, ChildSpecs}}.

get_child_specs() ->
	% CostRules = geek_track_settings:cost_rules(),
	BookingRules = geek_track_settings:booking_rules(),
	TrackInfoList = geek_track_settings:get_track_info_list(),

	lists:map(fun (TrackInfo) ->
		child_spec(TrackInfo, BookingRules)
	end, TrackInfoList).

child_spec(TrackInfo, BookingRules) ->
	#{rt_type := TrackType} = TrackInfo,

	#{
		id => TrackType,
		start => {geek_track_gen_server, start_link, [TrackInfo, BookingRules]},
		% start => {geek_track_server, start, [TrackInfo, BookingRules]},
		type => worker
	}.
