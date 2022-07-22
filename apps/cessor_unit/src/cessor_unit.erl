%%%-------------------------------------------------------------------
%% @doc
%%  Cessor Information Systems, CIS
%%  CIS License v0.1.0
%%  https://cessor.org
%%  Cessor Unit Application
%% @end
%%%-------------------------------------------------------------------

-module(cessor_unit).

-behaviour(application).

-export([start/2, stop/1]).

start(StartType, StartArgs) ->
    register(cessor_unit, self()),
    ENV=application:get_all_env(),
    io:format("StartType: ~p, StartArgs: ~p, ENV: ~p~n", [StartType, StartArgs, ENV]),
    {pin, PIN} = lists:keyfind(pin, 1, ENV),
    PIN_bin = binary:list_to_bin(PIN),
    %%  Test to read pin
    TX = <<
        <<"PIN: ">>/binary, PIN_bin/binary
    >>,
    Path = filename:join(".", "test00.txt"),
    file:write_file(Path, TX),

    
    application:unset_env(?MODULE, pin),

    %%  CSTP
    Version = {0,0,0},
    Service = {0,0},
    Procedure = {0,0},
    Process = {0,0},

    STATE = #{pin=>PIN_bin},
    try
        {ok, CSTX} = cstp:compose([Version, Service, Procedure, Process], STATE),
        io:format("CSTX: ~p~n", [CSTX]),
        cstp:compute(CSTX, STATE)
    catch
        exit:_ ->
            {error, cstp}
    end.

stop(_State) ->
    ok.