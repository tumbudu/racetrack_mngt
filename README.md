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


