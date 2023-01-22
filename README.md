GeekTrust RaceTrack Management
=====

An OTP application for problem (https://www.geektrust.com/coding/detailed/racetrack-management)

Build
-----

    $ rebar3 compile


Run
-----

    $ rebar3 shell


Run Test Cases 
-----

    $ rebar3 ct


Run default test 1
-----

    1> geek_input_file_processor:process_test1().


Run default test 2
-----

    1> geek_input_file_processor:process_test2().


Run custome file 
-----

    1> geek_input_file_processor:process_file(<Absolute File Path>).


e.g
    1> geek_input_file_processor:process_file("./priv/inputs/1.txt").





Code / Module summary
-----

Modules
racetrack_mngt_app -> Application
racetrack_mngt_sup -> Application Supervisor


racetrack_mngt_sup starts process
    1) track_sup -> geek_track_sup (supervisor for tracks)
    2) track_mgr -> geek_track_mgr (Genserver to serve booking request)

geek_track_sup -> Supervisor for tracks ( dynamically generated from geek_track_settings.erl)
    1) geek_track_gen_server for vip
    2) geek_track_gen_server for regular
    
geek_track_mgr -> Gen server to serve booking requests

helper_time -> Helper functions related to time
geek_track_settings -> Application rules in configurable format

geek_track_gen_server -> Gen server for each trace (1 For VIP and 1 For Regular)
gt_booking_handler -> Booking handler
geek_track_booking_helper -> Booking helper functions.

geek_input_file_processor -> For GeekTrust file processing


Note:
geek_track_mgr is bottelneck for large number of tracks.
You can directly call geek_track_gen_server for booking for respective track.
Refer geek_track_gen_server_SUITE.erl 
