-module(geek_track_gen_server_SUITE).

-export([
	all/0,

	test_regular_booking/1,
	test_additional_booking/1,
	test_invalid_entry_time/1,
	test_race_trace_full/1,
	test_additional_invalid_exit_time/1,
	test_additional_race_track_full/1,
	test_revenue_regular/1,
	test_revenue_regular_multiple/1,
	test_revenue_additional/1,
	test_revenue_multiple_additional/1
]).


all() ->[
	test_regular_booking,
	test_race_trace_full,

	test_additional_booking,
	test_additional_invalid_exit_time,
	test_additional_race_track_full,

	test_invalid_entry_time,
	
	test_revenue_regular,
	test_revenue_regular_multiple,
	test_revenue_additional,
	test_revenue_multiple_additional
].

%
% Regular bookings
%
test_regular_booking(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(vip),
	success = geek_track_gen_server:book(vip, suv, "C1", {13, 0, 0}).

test_race_trace_full(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(vip),
	success = geek_track_gen_server:book(vip, suv, "s1", {13, 0, 0}),
	success = geek_track_gen_server:book(vip, suv, "s2", {16, 0, 0}),
	race_trace_full = geek_track_gen_server:book(vip, suv, "s4", {17, 0, 0}).




%
% Test invalid entry time, out of booking times.
%
test_invalid_entry_time(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(regular),

	% save B1 same time double booking
	success = geek_track_gen_server:book(regular, bike, "B1", {13, 0, 0}),
	invalid_entry_time = geek_track_gen_server:book(regular, bike, "B1", {13, 0, 0}),

	% Before booking start time
	invalid_entry_time = geek_track_gen_server:book(regular, bike, "B2", {11, 0, 0}).



%
% Testing additional bookings
%
test_additional_booking(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(regular),
	success = geek_track_gen_server:book(regular, bike, "B1", {13, 0, 0}),
	success = geek_track_gen_server:additional(regular, "B1", {16, 30, 0}).

test_additional_invalid_exit_time(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(regular),

	% save B1 same time double booking
	success = geek_track_gen_server:book(regular, bike, "B1", {14, 0, 0}),
	invalid_exit_time = geek_track_gen_server:additional(regular, "B1", {13, 0, 0}).

test_additional_race_track_full(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(vip),
	success = geek_track_gen_server:book(vip, suv, "s1", {13, 0, 0}),
	success = geek_track_gen_server:book(vip, suv, "s2", {16, 0, 0}),
	race_trace_full = geek_track_gen_server:book(vip, suv, "s4", {14, 0, 0}).


%
% Testing revenue
%
test_revenue_regular(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(vip),
	success = geek_track_gen_server:book(vip, suv, "C1", {13, 0, 0}),
	300 = geek_track_gen_server:revenue(vip).

test_revenue_regular_multiple(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(vip),
	success = geek_track_gen_server:book(vip, suv, "C1", {13, 0, 0}), % 300
	success = geek_track_gen_server:book(vip, suv, "C2", {16, 0, 0}), % 300
	600 = geek_track_gen_server:revenue(vip).

test_revenue_additional(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(regular),
	success = geek_track_gen_server:book(regular, bike, "B1", {13, 0, 0}), % 60
	success = geek_track_gen_server:additional(regular, "B1", {16, 30, 0}), % 50
	110 = geek_track_gen_server:revenue(regular).


test_revenue_multiple_additional(_) ->
	application:ensure_all_started(racetrack_mngt),
	ok = geek_track_gen_server:reset(regular),
	success = geek_track_gen_server:book(regular, bike, "B1", {13, 0, 0}), % 60
	success = geek_track_gen_server:book(regular, bike, "B2", {14, 0, 0}), % 60
	success = geek_track_gen_server:additional(regular, "B1", {16, 30, 0}), % 50
	170 = geek_track_gen_server:revenue(regular).
