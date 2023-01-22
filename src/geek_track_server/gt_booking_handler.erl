-module(gt_booking_handler).

-export([
	handle_booking/2,
	calculate_revenue/1
]).

-define(D(X),  erlang:display({?MODULE, ?LINE, X})).


%
% handler functions
%
handle_booking({Vehicle, VId, StartTime, EndTime}=B, State) ->

	%
	% Booking rules
	%
	BookingRules = maps:get(booking_rules, State),

	case geek_track_booking_helper:is_valid_entry_time(BookingRules, StartTime, EndTime) of
		true -> ok;
		_ -> throw(invalid_entry_time)
	end,

	%
	% Get booking list
	%
	Bookings = maps:get(bookings, State),
	VBookings = proplists:get_value(Vehicle, Bookings, []),

	%
	% Get already allocated booking timing for VId and Check for invalid time
	%
	IsValidBooking = geek_track_booking_helper:is_valid_booking(VId, VBookings, StartTime, EndTime),

	case IsValidBooking of
		true -> skip;
		false -> throw(invalid_entry_time)
	end,

	%
	% 2. If not already allocated then find slot and allocate booking
	%
	{AllocationStatus, FVBookings} = lists:foldl(fun
		(VB, {false, AccVB}) ->
			Cost = geek_track_booking_helper:get_cost(Vehicle, StartTime, EndTime, State),
			{IsAllocated, VB1} = geek_track_booking_helper:allocate_booking(VB, B, Cost),
			{IsAllocated, [VB1|AccVB]};
		(VB, {true, AccVB}) ->
			{true, [VB|AccVB]}
	end, {false, []}, VBookings),

	case AllocationStatus of
		true ->
			Bookings1 = lists:keyreplace(Vehicle, 1, Bookings, {Vehicle, FVBookings}),
			{success ,State#{bookings:=Bookings1}};
		false ->
			{not_allocated, State}
	end.


calculate_revenue(_State) ->
	0.
