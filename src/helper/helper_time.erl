-module(helper_time).
-export([
	time_add/2,
	time_diff/2,
	get_duration_in_hrs/1,
	is_overlapping/4,
	is_in_middle/3,
	format_time/1
]).

time_add({H1, M1, S1}, {H2, M2, S2}) ->
	S = S1 + S2,
	M = M1 + M2 + (S div 60),
	H = H1 + H2 + (M div 60),
	{H rem 24, M rem 60, S rem 60}.

time_diff({H1, M1, S1}, {H2, M2, S2}) ->
	S = S1 - S2,
	M = M1 - M2 + (S div 60),
	H = H1 - H2 + (M div 60),
	{H rem 24, M rem 60, S rem 60}.


get_duration_in_hrs({Hr, 0, 0}) -> Hr;
get_duration_in_hrs({Hr, _Min, _Sec}) -> Hr + 1.


is_overlapping(STime, ETime, StartTime, EndTime) ->
	is_in_middle(StartTime, STime, ETime) or
	is_in_middle(EndTime, STime, ETime).


is_in_middle(STime, STime, _ETime) -> false;
is_in_middle(ETime, _STime, ETime) -> false;
is_in_middle(BTime, STime, ETime) ->
	case lists:sort([BTime, STime, ETime]) of
		[_, BTime, _] -> true;
		_ -> false
	end.

format_time(Time) ->
	[Hr, Min] = string:tokens(Time, ":"),
	{list_to_integer(Hr), list_to_integer(Min), 0}.
	