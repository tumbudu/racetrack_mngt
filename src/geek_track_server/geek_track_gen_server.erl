-module(geek_track_gen_server).
-behaviour(gen_server).
-export([start_link/2]).


%% gen_server callbacks
-export([init/1,
	 handle_call/3,
	 handle_cast/2,
	 handle_info/2,
	 terminate/2,
	 code_change/3]).

%
% apis
%
-export([
	reset/1,
	book/4,
	additional/3,
	revenue/1
]).



start_link(TrackInfo, BookingRules) ->
	ServerName = maps:get(rt_type, TrackInfo),
	gen_server:start_link({local, ServerName}, ?MODULE, [TrackInfo, BookingRules], []).

%
% init gen_server
%
init([TrackInfo, BookingRules]) ->
	State = init_state(TrackInfo, BookingRules),
	{ok, State}.

%
% Gen server handlers
%
handle_call({book, {Vehicle, VId, StartTime}}, _From, State) ->
	handle_booking(Vehicle, VId, StartTime, State);

handle_call({additional, {VId, EndTime}}, _From, State) ->
	handle_additional_booking(VId, EndTime, State);

handle_call(revenue, _From, State) ->
	Msg = gt_booking_handler:calculate_revenue(State),
	{reply, Msg, State};

handle_call(reset, _From, State) ->
	{reply, ok, reset_state(State)};

handle_call(_Request, _From, State) ->
	{reply, ignored, State}.

handle_cast(_Msg, State) ->
	{noreply, State}.

handle_info(_Info, State) ->
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.


%
% APIs
%
book(TrackId, Vehicle, VId, StartTime) ->
	gen_server:call(TrackId, {book, {Vehicle, VId, StartTime}}).

additional(TrackId, VId, EndTime) ->
	gen_server:call(TrackId, {additional, {VId, EndTime}}).

revenue(TrackId) ->
	gen_server:call(TrackId, revenue).

reset(TrackId) ->
	gen_server:call(TrackId, reset).	

%
% Init state
%

init_state(TrackInfo, BookingRules) ->
#{
	trackid => maps:get(rt_type, TrackInfo),
	booking_rules => BookingRules,
	allowed_cars => maps:get(allowed_cars, TrackInfo),
	bookings => init_bookings(maps:get(number_of_vehicles, TrackInfo)),

	rate => maps:get(rate, TrackInfo),
	additional_charges => maps:get(additional_charges, TrackInfo),
	min_minutes_for_additional_booking => maps:get(min_minutes_for_additional_booking, TrackInfo)
}.

init_bookings(Vehicles) ->
	lists:map(fun({Type, Ctr}) ->
		{Type, [[] || _N <- lists:seq(1, Ctr)]}
	end, Vehicles).


%
% This is for testing only..
%
reset_state(State) ->
	Bookings = maps:get(bookings, State),

	EmptyBookings = lists:map(fun({Type, List}) ->
		{Type, [[] || _I <- List]}
	end, Bookings),

	State#{
		bookings := EmptyBookings
	}.


%
% Handlers
%
handle_booking(Vehicle, VId, StartTime, State) ->

	BookingRules = maps:get(booking_rules, State),
	MinBookingTime = maps:get(min_booking_hrs, BookingRules),

	EndTime = get_end_time(StartTime, MinBookingTime),
	BookingDetails = {Vehicle, VId, StartTime, EndTime},
	try
		{Msg, State1} = gt_booking_handler:handle_booking(BookingDetails, State),
		{reply, Msg, State1}
	catch
		_T:E:_Stacktrace ->
			% erlang:display([T,E,Stacktrace]),
			{reply, E, State}
	end.

handle_additional_booking(VId, EndTime, State) ->
	case get_bookings(VId, State) of
		{true, {V, BList}} ->
			case get_nearest_endtime(BList, EndTime) of
				{_, undefined} ->
					{reply, invalid_exit_time, State};

				{true, STime} ->
					BookingDetails = {V, VId, STime, EndTime},
					try
						{Msg, State1} = gt_booking_handler:handle_booking(BookingDetails, State),
						{reply, Msg, State1}
					catch
						_T:E:_Stacktrace ->
							% erlang:display([T,E,Stacktrace]),
							{reply, E, State}
					end;

				{false, _} ->
					{reply, invalid_exit_time, State}
			end;
		_ ->
			{reply, booking_not_found, State}
	end.


%
% Misc functions
%

get_end_time(StartTime, MinBookingTime) ->
	helper_time:time_add(StartTime, MinBookingTime).

get_bookings(VId, State) ->
	Bookings = maps:get(bookings, State),

	lists:foldl(fun
		({V, BList}, {false, Acc}) ->
			case proplists:get_all_values(VId, lists:flatten(BList)) of
				[] -> {false, Acc};
				List -> {true, {V, List}}
			end;
		(_, {true, Acc}) ->
			{true, Acc}
	end, {false, []}, Bookings).

get_nearest_endtime(BList, EndTime) ->
	ETimeList = [ETime || {_STime, ETime, _} <- BList],
	SortedTList = lists:sort(ETimeList ++ [EndTime]),
	FTList = lists:filter(fun
		(T) when T =< EndTime -> true;
		(_T) -> false
	end, SortedTList),
	lists:foldl(fun
		(Time, {false, LTime}) when Time == EndTime ->  {true, LTime};
		(Time, {false, _LTime}) -> {false, Time};
		(_, {true, LTime}) -> {true, LTime}
	end, {false, undefined}, FTList).
