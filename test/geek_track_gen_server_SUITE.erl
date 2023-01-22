-module(geek_track_gen_server_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
	all/0,
	test_regular_booking/1,
	test_additional_booking/1
]).


all() ->[
	test_regular_booking,
	test_additional_booking
].


test_regular_booking(_) ->
	application:ensure_all_started(racetrack_mngt),
	success = geek_track_gen_server:book(vip, suv, "C1", {11, 0, 0}).

test_additional_booking(_) ->
	application:ensure_all_started(racetrack_mngt),
	success = geek_track_gen_server:book(regular, bike, "B1", {10, 0, 0}),
	success = geek_track_gen_server:additional(regular, "B1", {14, 30, 0}).
