-module(geek_track_settings).

-export([
	get_track_info_list/0,
	booking_rules/0,
	cost_rules/0
]).


get_track_info_list() -> [
	#{
		rt_type => regular,
		allowed_cars => [bike, car, suv],
		number_of_vehicles => [{bike, 4}, {car, 2}, {suv, 2}],
		rate => [{bike, 60}, {car, 120}, {suv, 200}],
		additional_charges => [{bike, 50}, {car, 50}, {suv, 50}],
		min_minutes_for_additional_booking => [{bike, {0, 16, 0}}, {car, {0, 16, 0}}, {suv, {0, 16, 0}}]

	},
	#{
		rt_type => vip,
		allowed_cars => [suv, car],
		number_of_vehicles => [{suv, 1}, {car, 1}],
		rate => [{suv, 300}, {car, 250}],
		additional_charges => [{bike, 50}, {car, 50}, {suv, 50}],
		min_minutes_for_additional_booking => [{bike, {0, 16, 0}}, {car, {0, 16, 0}}, {suv, {0, 16, 0}}]
	}
].


booking_rules() ->
	#{
		start_time => {10, 0, 0},
		end_time => {20, 0, 0},
		min_booking_hrs => {3, 0, 0},
		track_order => [regular, vip]
	}.

cost_rules() ->
	#{
		cost => [{bike, 60}, {car, 120}, {suv, 200}, {suv, 300}],
		additional_charges => [{50, [bike, car, suv]}],
		min_minutes_for_additional_booking => [{16, [bike, car, suv]}]
	}.

