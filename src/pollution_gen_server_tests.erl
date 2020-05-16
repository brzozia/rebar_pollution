%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. kwi 2020 20:17
%%%-------------------------------------------------------------------
-module(pollution_gen_server_tests).
-author("Natalia Brzozowska").

-include_lib("eunit/include/eunit.hrl").
-export([start_test/0,stop_test/0,addStation_test/0,addValue_test/0,getOneValue_test/0,getStationAndDailyMean_test/0,removeValueAndOther_test/0]).


start_test() ->
  {A,_B}=pollution_gen_server:start(),
  ?assertEqual(error,A),                 % when program is started using rebar, supervisor starts pollution_gen_server, so one server is already started when whe (now) want to start a new one
  {B,_B}=pollution_gen_server:start(),
  ?assertEqual(error,B).

stop_test() ->
  ?assertEqual(stopped,pollution_gen_server:stop()).


%% those tests checks receiving and sending messages between processes (but also checks a little of pollution module logic)

addStation_test() ->
  pollution_gen_server:start(),
  ?assertEqual('station added', pollution_gen_server:addStation("Kra",{123.234,23.23})),
  ?assertEqual(same_station_attributes, pollution_gen_server:addStation("Bronowice",{123.234,23.23}) ), %% same coordinates as before
  ?assertEqual(same_station_attributes, pollution_gen_server:addStation("Kra",{187.234,29.23})),   %% same name as before
  pollution_gen_server:stop().

addValue_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Kra",{123.234,23.23}),
  ?assertEqual('value added', pollution_gen_server:addValue("Kra", calendar:local_time(),"PM10", 123) ),
  ?assertEqual('value added',pollution_gen_server:addValue({123.234,23.23}, {{2020,04,21},{11,11,11}},"PM2.5", 10)),
  ?assertEqual(same_values_to_station,pollution_gen_server:addValue("Kra", {{2020,04,21},{11,11,11}},"PM2.5", 10) ), %% same values as before - cannot add
  ?assertEqual(wrong_station_name,pollution_gen_server:addValue("Zabierzow", calendar:local_time(),"PM2.5", 13) ), %% adds value to unavailable station - cannot
  pollution_gen_server:stop().

removeValueAndOther_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Kra",{123.234,23.23}),
  ?assertEqual('value added', pollution_gen_server:addValue("Kra", {{2020,04,21},{11,11,11}},"Temp", 10) ),
  ?assertEqual('value removed',pollution_gen_server:removeValue("Kra", {{2020,04,21},{11,11,11}},"Temp")), %% removes just added value
  ?assertEqual(no_such_value,pollution_gen_server:getOneValue("Kra", {{2020,04,21},{11,11,11}},"Temp")), %% checks whether the value is in monitor - it is not
  ?assertEqual(no_such_value,pollution_gen_server:removeValue("Kra", {{2020,04,21},{11,11,11}},"Temp") ), %% tries again to remove this value - cannot
  pollution_gen_server:stop().

getOneValue_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Kra",{123.234,23.23}),
  pollution_gen_server:addValue("Kra", {{2020,04,21},{11,11,11}},"PM10", 123),
  pollution_gen_server:addValue("Kra", {{2020,04,21},{11,11,12}},"PM2.5", 10),
  pollution_gen_server:addStation("Bronowice", {31.23,45.67}),
  pollution_gen_server:addValue("Bronowice", {{2020,04,22},{11,11,11}}, "Temp", 15),

  ?assertEqual(123,pollution_gen_server:getOneValue("Kra", {{2020,04,21},{11,11,11}},"PM10")), %% gets correct values
  ?assertEqual(10,pollution_gen_server:getOneValue("Kra", {{2020,04,21},{11,11,12}},"PM2.5")),
  ?assertEqual(15,pollution_gen_server:getOneValue("Bronowice", {{2020,04,22},{11,11,11}}, "Temp")),

  ?assertEqual(no_such_value,pollution_gen_server:getOneValue("Kra", {{2020,04,21},{11,11,10}},"PM10")), %% gets not available data - cannot
  ?assertEqual(no_such_value,pollution_gen_server:getOneValue("Bronowice", {{2020,04,21},{11,11,11}},"Temp")),
  ?assertEqual(wrong_station_name,pollution_gen_server:getOneValue("Zabierzow", {{2020,04,21},{11,11,11}},"Temp")), %% gets data from station which does not exist
  pollution_gen_server:stop().

getStationAndDailyMean_test() ->
  pollution_gen_server:start(),
  pollution_gen_server:addStation("Bronowice", {31.23,45.67}),
  pollution_gen_server:addValue({31.23,45.67}, {{2020,04,21},{11,11,10}}, "PM10", 20),
  pollution_gen_server:addValue("Bronowice", {{2020,04,21},{11,11,10}}, "PM10", 10),
  pollution_gen_server:addStation("Kra",{123.234,23.23}),
  pollution_gen_server:addValue("Kra", {{2020,04,21},{11,11,10}},"PM10", 3),
  pollution_gen_server:addValue("Kra", {{2020,04,21},{11,11,10}},"Temp", 10),

  ?assertEqual(15.0,pollution_gen_server:getStationMean("Bronowice","PM10")),
  ?assertEqual(11.0,pollution_gen_server:getDailyMean({2020,04,21},"PM10")),
  ?assertNotEqual(15.0,pollution_gen_server:getStationMean("Bronowice","PM2.5")),
  ?assertNotEqual(15.0,pollution_gen_server:getStationMean("Kra","Temp")),

  %% getWorstDay test
  pollution_gen_server:addValue({31.23,45.67}, {{2020,04,22},{11,11,10}}, "PM10", 25),
  pollution_gen_server:addValue("Bronowice", {{2020,04,22},{11,11,10}}, "PM10", 10),
  ?assertEqual({{{2020,04,22},{11,11,10}},25},pollution_gen_server:getWorstDay("Bronowice","PM10")),
  ?assertNotEqual({{{{2020,04,22},{11,11,10}}},25.0},pollution_gen_server:getWorstDay("Bronowice","Temp")),

  %% getWorstHourlyStation test
  ?assertEqual({"Bronowice", 25},pollution_gen_server:getWorstHourlyStation({2020,04,22},11,"PM10")),
  pollution_gen_server:stop().


