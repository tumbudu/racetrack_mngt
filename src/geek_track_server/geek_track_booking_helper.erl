-module(geek_track_booking_helper).
-export([
	is_valid_entry_time/3,
	is_valid_booking/4,
	get_cost/4,
	allocate_booking/3
]).

%
% Allocate booking B = {book, Vehicle, VId, StartTime, EndTime}
%
allocate_booking(VBList, {_Vehicle, VId, StartTime, EndTime}, Cost) ->
	case is_slot_available(VBList, StartTime, EndTime) of
		true ->
			{true, VBList ++ [{VId, {StartTime, EndTime, Cost}}]};
		_ -> {false, VBList}
	end.


%
% validations
%
is_valid_entry_time(BookingRules, StartTime, EndTime) ->
	BookingStartTime = maps:get(start_time, BookingRules),
	BookingEndTime = maps:get(end_time, BookingRules),

	A = (StartTime >= BookingStartTime) and (StartTime =< BookingEndTime),
	B = (EndTime >= BookingStartTime) and (EndTime =< BookingEndTime),
	A and B.

is_slot_available(VBList, StartTime, EndTime) ->
	BookedSlots = [VB || {_Id, VB} <- VBList],
	lists:foldl(fun
		(_BS, false) ->
			false;

		(BS, true) ->
			{STime, ETime, _Cost} = BS,
			IsOverlapping = helper_time:is_overlapping(STime, ETime, StartTime, EndTime),
			(not IsOverlapping) and (StartTime =/= STime)
	end, true, BookedSlots).

is_valid_booking(VId, VBookings, StartTime, EndTime) ->
	VIdBookings = lists:foldl(fun(BList, Acc) ->
		Acc ++ proplists:get_all_values(VId, BList)
	end, [], VBookings),
	is_valid_booking(VIdBookings, StartTime, EndTime).

is_valid_booking([], _StartTime, _EndTime) ->
	true;
is_valid_booking(VIdBookings, StartTime, EndTime) ->
	lists:foldl(fun
		(B, true) ->
			{STime, ETime, _Cost} = B,
			(not helper_time:is_overlapping(STime, ETime, StartTime, EndTime)) and (STime =/= StartTime);
		(_B, false) ->
			false
	end, true, VIdBookings).

get_booking_type(StartTime, EndTime, State) ->
	BookingRules = maps:get(booking_rules, State),
	MinBookingTime = maps:get(min_booking_hrs, BookingRules),

	case helper_time:time_diff(EndTime, StartTime) of
		MinBookingTime -> regular;
		_ -> additional
	end.

get_rates(regular, State) -> maps:get(rate, State);
get_rates(additional, State) -> maps:get(additional_charges, State).



%
%  calculate cost
%
get_cost(Vehicle, StartTime, EndTime, State) ->
	BookingType = get_booking_type(StartTime, EndTime, State),
	RateList = get_rates(BookingType, State),
	BookingDuration = helper_time:time_diff(EndTime, StartTime),
	MinDurationOfBooking = proplists:get_value(Vehicle, maps:get(min_minutes_for_additional_booking, State), 1),

	get_cost(BookingType, Vehicle, RateList, BookingDuration, MinDurationOfBooking).

get_cost(regular, Vehicle, RateList, _BookingDuration, _MinDurationOfBooking) ->
	3 * proplists:get_value(Vehicle, RateList);

get_cost(additional, _Vehicle, _RateList, BookingDuration, MinDurationOfBooking) when BookingDuration < MinDurationOfBooking ->
	0;

get_cost(additional, Vehicle, RateList, BookingDuration, _MinDurationOfBooking) ->
	PerHrCost = proplists:get_value(Vehicle, RateList),
	HrsDuration = helper_time:get_duration_in_hrs(BookingDuration),
	PerHrCost * HrsDuration.
