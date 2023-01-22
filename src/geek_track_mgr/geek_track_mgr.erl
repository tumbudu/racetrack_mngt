-module(geek_track_mgr).
-behaviour(gen_server).

-export([start_link/0]).

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
	reset/0,
	book/3,
	additional/2,
	revenue/0
]).



	start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%
% init gen_server
%
init([]) ->
	{ok, []}.

%
% Gen server handlers
%
handle_call({book, {Vehicle, VId, StartTime}}, _From, State) ->
	Msg = handle_booking(Vehicle, VId, StartTime),
	{reply, Msg, State};

handle_call({additional, {VId, EndTime}}, _From, State) ->
	Msg = handle_additional_booking(VId, EndTime),
	{reply, Msg, State};

handle_call(revenue, _From, State) ->
	Msg = handle_calculate_revenue(),
	{reply, Msg, State};

handle_call(reset, _From, State) ->
	Msg = handle_reset(),
	{reply, Msg, State};


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
reset() ->
    gen_server:call(?MODULE, reset).
revenue() ->
    gen_server:call(?MODULE, revenue).
book(Vehicle, VId, StartTime) ->
	gen_server:call(?MODULE, {book, {Vehicle, VId, StartTime}}).

additional(VId, EndTime) ->
	gen_server:call(?MODULE, {additional, {VId, EndTime}}).
	



%
% Handlers
%
handle_reset() ->
	ok = geek_track_gen_server:reset(vip),
	ok = geek_track_gen_server:reset(regular).

handle_calculate_revenue() ->
	VAmt = geek_track_gen_server:revenue(vip),
	RAmt = geek_track_gen_server:revenue(regular),
	{RAmt, VAmt}.
	

handle_booking(Vehicle, VId, StartTime) ->
	case geek_track_gen_server:book(regular, Vehicle, VId, StartTime) of
		success -> success;
		E -> 
			case geek_track_gen_server:book(vip, Vehicle, VId, StartTime) of
				success -> success;
				_ -> E
			end
	end.
handle_additional_booking(VId, EndTime) ->
	case geek_track_gen_server:additional(regular, VId, EndTime) of
		success -> success;
		E -> 
			case geek_track_gen_server:book(vip, regular, VId, EndTime) of
				success -> success;
				_ -> E
			end
	end.