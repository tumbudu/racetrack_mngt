-module(geek_input_file_processor).
-export([
	process_file/1,
	process_test1/0,
	process_test2/0
]).

%
% Run test input
%
process_test1() -> 
	geek_track_mgr:reset(),
	process_file("./priv/inputs/1.txt").
process_test2() -> 
	geek_track_mgr:reset(),
	process_file("./priv/inputs/2.txt").

process_file(FPath) ->
	{ok, File} = file:open(FPath, [read]),
	process_lines(File).

process_lines(File) ->
	case file:read_line(File) of
		eof ->
			file:close(File),
			ok;
		{ok, "\n"} ->
			ok;
		{ok, ""} ->
			ok;
		{ok, Line} ->
			Result = process_cmd([string:to_lower(S) || S <- string:tokens(string:trim(Line), " ")]),
			print_result(Result),
			process_lines(File)
	end.

%
% process command
%	
process_cmd(["book", Vehical, VId, Time]) ->
	FTime = helper_time:format_time(Time),
	geek_track_mgr:book(list_to_atom(Vehical), VId, FTime);

process_cmd(["additional", VId, Time]) ->
	FTime = helper_time:format_time(Time),
	geek_track_mgr:additional(VId, FTime);

process_cmd(["revenue"]) ->
	geek_track_mgr:revenue().
	

%
% print formated result
%
print_result(success) -> io:format("~s~n", ["SUCCESS"]);
print_result(invalid_entry_time) -> io:format("~s~n", ["INVALID_ENTRY_TIME"]);
print_result(invalid_exit_time) -> io:format("~s~n", ["INVALID_EXIT_TIME"]);
print_result(race_trace_full) -> io:format("~s~n", ["RACETRACK_FULL"]);
print_result({RAmt, VAmt}) -> io:format("~s ~s~n", [integer_to_list(RAmt), integer_to_list(VAmt)]);
print_result(Error) -> io:format("~s~n", [atom_to_list(Error)]).
