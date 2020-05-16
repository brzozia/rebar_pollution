%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. kwi 2020 04:57
%%%-------------------------------------------------------------------
-module(pollution).
-author("Natalia Brzozowska").

%% API
-export([createMonitor/0, addStation/3, reallyAddStation/3,addValue/5,removeValue/4,getOneValue/4, getStationMean/3,
  getDailyMean/3,getTuples/5, existMeasure/3, getWorstDay/3,compareMeasurements/3,getWorstHourlyStation/4,findWorstStation/7,
  compareMeasurementsHourly/4]).


%%-----------------------data structure---------------------------
%%
%% 'monitor' is a record which contains two dictionaries:
%% -> first one - 'stations' - contains stations' data: key is a station's coordinates, and value is a station's name
%% -> second one - 'stationsData' - is a main dictionary - key is station's name and value is a record 'stationData',
%%  containing station's coordinates and list of measurements
%% list of measurements contains 'measurement' records
%%

-record(monitor, {stations, stationsData}).
-record(stationData, {coordinates, measurements=[]}).
-record(measurement, {type, value, date}).

%%-----------------------functions---------------------------------
%%-----------createMonitor----------
%% 'createMonitor' is a function which creates monitor containing two dictionaries.

createMonitor() -> #monitor{stations=dict:new(), stationsData=dict:new()}.


%%------------addStation------------
%% 'addStation' is a function which firstly checks whether station already exist in monitor, and if not
%% calls 'reallyAddStation'  which adds station's data to both monitor's dictionaries, and returns new updated monitor.

addStation(Monitor, Name, Coordinates)  ->
  case ( dict:is_key(Name,  Monitor#monitor.stationsData) or
    dict:is_key(Coordinates,  Monitor#monitor.stations)) of
    true ->  {error,same_station_attributes, Monitor};
    false -> reallyAddStation(Monitor, Name, Coordinates)
  end.

reallyAddStation(Monitor, Name, Coordinates) ->
  StationsDict = dict:store(Coordinates, Name, Monitor#monitor.stations),
  StationsDataDict = dict:store(Name, #stationData{coordinates = Coordinates, measurements = []}, Monitor#monitor.stationsData),
  {reply,'station added',#monitor{stations=StationsDict, stationsData=StationsDataDict}}.


%%------------addValue------------
%% 'addValue' is a function, which adds value to monitor to given dictionary key,
%%  after checking whether there already is such measurement in monitor's dictionary.
%% It adds Measure to measurements' list
%% It calls functions 'existMeasure' and 'getStationName'.

addValue(Monitor, Id, Date, Type, Value ) ->
  Measure = #measurement{type=Type, value =Value, date=Date},
  try getStationName(Monitor,Id) of
    Name -> case existMeasure(Monitor,Name,Measure) of
              true -> {error,same_values_to_station,Monitor};
              false -> {reply, 'value added', #monitor{stations= Monitor#monitor.stations,
                stationsData=dict:update(Name,
                  fun(Old) -> #stationData{coordinates = Old#stationData.coordinates,
                    measurements = Old#stationData.measurements ++[Measure]}
                  end,
                  Monitor#monitor.stationsData)}}
            end
  catch
    error:wrong_station_name -> {error,wrong_station_name,Monitor}
  end.


%% Function 'existMeasure' check whether the measure, which we want to add already exist in monitor.

existMeasure(Monitor,Name,Measure) ->
  {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
  List = lists:filter(
    fun
      (Elem) when Elem==Measure -> true;
      (_)->false end, Measurements),
  case List of
    [] -> false;
    _ -> true
  end.

%% Function 'getStationName' returns name of station basing on given Id. Id can be a station's coordinates or name.

getStationName(Monitor, Id) when is_tuple(Id) ->
  case dict:is_key(Id,Monitor#monitor.stations) of
    true -> {_,Name} = dict:find(Id, Monitor#monitor.stations), Name; %% if Station does not exist in dict, dict:find returns error
    _ -> error(wrong_station_name)
  end;
getStationName(Monitor, Id) ->
  case dict:is_key(Id,Monitor#monitor.stationsData) of
    true -> Name=Id,Name;
    _ -> error(wrong_station_name)
  end.


%%------------removeValue------------
%% This function removes a value connected to given station's Id and specified Date and Type of measurement.
%% It updates the dictionary, removing value from measurements' list.

removeValue(Monitor, Id, Date, Type) ->
  try getStationName(Monitor,Id) of
    Name ->
      {At, Val}=getOneValue(Monitor, Id, Date, Type),
      case At of
        error -> {At,Val,Monitor};
        reply ->{reply,'value removed',#monitor{stations= Monitor#monitor.stations,
          stationsData=dict:update(Name,
            fun(Old) ->  #stationData{coordinates = Id,
              measurements = lists:delete(#measurement{type=Type,date=Date, value=Val},
                Old#stationData.measurements) }
            end,
            Monitor#monitor.stationsData)}}
      end
  catch
    error:wrong_station_name -> {error,wrong_station_name,Monitor}
  end.


%%------------getOneValue------------
%% Returns value from specified measurement of specified station, basing on station's Id.
%% It gets list of measurements of the station and then filters this list.

getOneValue(Monitor, Id, Date, Type) ->
  try getStationName(Monitor,Id) of
    Name -> {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
      List= lists:filter(
        fun
          (Elem) when (Elem#measurement.type==Type) and (Elem#measurement.date==Date) -> true;
          (_)->false
        end, Measurements),
      case List of
        [] -> {error,no_such_value};
        [{_,_,Val,_}] ->  {reply,Val}
      end
  catch
    error:wrong_station_name -> {error,wrong_station_name}
  end.



%%------------getStationMean------------
%% Function 'getStationMean' returns mean of all measurements of specified type of given station.
%% Firstly it gets list of measurements of the station and then filters this list.
%% After that it uses foldl to compute the mean.

getStationMean(Monitor, Id, Type) ->
  try getStationName(Monitor,Id) of
    Name ->  {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
      TypeList = lists:filter(
        fun
          (Elem) when (Elem#measurement.type==Type) -> true;
          (_)->false
        end, Measurements),
      case TypeList of
        [] -> {reply,0.0};
        _ -> {reply,lists:foldl(fun(X, Sum) -> X#measurement.value + Sum end, 0, TypeList) / lists:foldl(fun(_, Sum) -> 1 + Sum end, 0, TypeList)}
      end
  catch
    error:wrong_station_name -> {error,wrong_station_name}
  end.





%%------------getDailyMean------------
%% Day should be a date() tuple - {year,month,day}
%% Function returns a mean of all measurements of specified type and day from all stations.
%% Firstly it gets all stations' names and then, using 'getTuples' all tuples containing that day's measurements.
%% After computes mean using foldl.

getDailyMean(Monitor, Day,Type) ->
  Stations = dict:fetch_keys(Monitor#monitor.stationsData),
  TuplesList = getTuples(Monitor, Stations, Day, Type, []),
  No = lists:foldl(fun(_, Sum) -> 1 + Sum end, 0, TuplesList),
  case No of
    0 -> {reply,0.0};
    _ -> {reply,lists:foldl(fun(X, Sum) -> X#measurement.value + Sum end, 0, TuplesList) / No}
  end.



%% 'getTuples' returns a list of tuples, which contains measurements from given day.
%% Function search whole monitor dictionary to find that tuples and adds them to final list.

getTuples(_, [], _, _, TuplesList) ->
  TuplesList;
getTuples(Monitor, [H | Stations], DateV, Type, TuplesList) ->
  {_,_, Measurements}= dict:fetch(H, Monitor#monitor.stationsData),
  NewTuplesList = TuplesList ++ lists:filter(
      fun
      (Elem) when (Elem#measurement.type==Type) -> {Date, _} = Elem#measurement.date, Date==DateV;
      (_)->false
      end,Measurements),
  getTuples(Monitor,Stations, DateV, Type, NewTuplesList).


%%------------getWorstDay------------
%% Function 'getWorstDay' returns the worst daytime and value of all measurements from given type and given station.
%% It makes list of measurements of given type and from given station and calls 'compareMeasurements' with this list.

getWorstDay(Monitor, Id, Type) ->
  try getStationName(Monitor,Id) of
    Name ->  {_,_, Measurements}= dict:fetch(Name, Monitor#monitor.stationsData),
      TypeMeasurements = lists:filter(
        fun
          (Elem) when (Elem#measurement.type==Type) -> true;
          (_)->false
        end,Measurements),
      compareMeasurements(TypeMeasurements,{}, 0 )
  catch
    error:wrong_station_name -> {error,wrong_station_name}
  end.



%% 'compareMeasurements' compares measurements in given list.
%% It returns datetime of the measurement with the highest value and this value.

compareMeasurements([],WorstDate, WorstVal) ->
  {reply,{WorstDate, WorstVal}};
compareMeasurements([H | Tail],WorstDate, WorstVal) ->
  case H#measurement.value > WorstVal of
    true -> compareMeasurements(Tail,H#measurement.date, H#measurement.value);
    false -> compareMeasurements(Tail,WorstDate, WorstVal)
  end.


%%------------getWorstHourlyStation------------
%% Function 'getWorstHourlyStation' returns station's name and worst value of given type in given hour.
%% If function returns {0,0}, that means that there is no measurements in that hour.

getWorstHourlyStation(Monitor, Day, Hour, Type) ->
  [H |Stations] = dict:fetch_keys(Monitor#monitor.stationsData),
  findWorstStation(Monitor, [H |Stations], Day, Hour, Type, 0 ,0).



%% 'findWorstStation' is similar to 'getTuples' but checks also hour of measurement and returns worst station's name.
%% It gets measurements' list from each station and  calls 'compareMeasurementsHourly',
%% which compare values of given type with actual worst value.

findWorstStation(Monitor, [H | Stations], DateV, HourV, Type, WorstStationName, WorstValue) ->
  {_,_, Measurements}= dict:fetch(H, Monitor#monitor.stationsData),
  NewTuplesList =  lists:filter(
    fun
      (Elem) when (Elem#measurement.type==Type) -> {Date, {Hour,_,_}} = Elem#measurement.date, (HourV==Hour) and (Date==DateV) ;
      (_)->false
    end,Measurements),
  {NewWorstStation, NewWorstValue} = compareMeasurementsHourly(NewTuplesList,WorstStationName, WorstValue, H),
  findWorstStation(Monitor,Stations, DateV,HourV, Type,NewWorstStation,NewWorstValue);
findWorstStation(_, [], _, _,_,WorstStationName,WorstValue) ->
  {reply,{WorstStationName,WorstValue}}.


%% Compares values from list of given type with actual worst value.

compareMeasurementsHourly([],WorstStation,WorstValue,_) ->
  {WorstStation, WorstValue};
compareMeasurementsHourly([H | Tail],WorstStationName,WorstValue,ActStation) ->
  case H#measurement.value >= WorstValue of
    true -> compareMeasurementsHourly(Tail,ActStation, H#measurement.value,ActStation );
    false -> compareMeasurementsHourly(Tail,WorstStationName, WorstValue,ActStation)
  end.


%%test:

%%M = pollution:createMonitor().
%%
%%M2 = pollution:addStation(M, "Bronowice", {31.23,45.67}).
%%M4 = pollution:addValue(M2, {31.23,45.67}, calendar:local_time(), "PM10", 23.4).
%%M5 = pollution:addValue(M4, "Bronowice", calendar:local_time(), "PM2.5", 40.4).
%%M6 = pollution:addValue(M5, "Bronowice", calendar:local_time(), "Temp", 15).
%%
%%pollution:getOneValue(M6,"Bronowice", calendar:local_time(), "PM10").
%%
%%M7 = pollution:addStation(M6, "Azory", {50.2345, 18.3445}).
%%M8 = pollution:addValue(M7, {50.2345, 18.3445}, calendar:local_time(), "PM10", 50.7).
%%M9 = pollution:addValue(M8, "Azory", calendar:local_time(), "PM2.5", 72.4).
%%M10 = pollution:addValue(M9, "Azory", calendar:local_time(), "Temp", 16.1).
%%
%%pollution:getOneValue(M10,{50.2345, 18.3445}, calendar:local_time(), "PM10").
%%pollution:getOneValue(M10,"Bronowice", calendar:local_time(), "PM10").
%%
%%
%%M11 = pollution:addStation(M10, "Dietla", {120.1234, 10.5678}).
%%M12 = pollution:addValue(M11, {120.1234, 10.5678}, calendar:local_time(), "PM10", 120.7).
%%M13 = pollution:addValue(M12, "Dietla", calendar:local_time(), "PM2.5", 100).
%%M14 = pollution:addValue(M13, "Dietla", calendar:local_time(), "Temp", 16.5).
%%
%%pollution:getOneValue(M14,{31.23,45.67}, calendar:local_time(), "PM2.5").
%%pollution:getOneValue(M14,"Azory", calendar:local_time(), "Temp").
%%pollution:getOneValue(M14,"Dietla", calendar:local_time(), "PM10").
%%
%%M15 = pollution:addValue(M14, {31.23,45.67}, calendar:local_time(), "PM10", 10.1).
%%M16 = pollution:addValue(M15, "Bronowice", calendar:local_time(), "PM2.5", 34.45).
%%M17 = pollution:addValue(M16, "Bronowice", calendar:local_time(), "Temp", 15.1).
%%M18 = pollution:addValue(M17, {50.2345, 18.3445}, calendar:local_time(), "PM10", 50.0).
%%M19 = pollution:addValue(M18, "Azory", calendar:local_time(), "PM2.5", 43.3).
%%M20 = pollution:addValue(M19, "Azory", calendar:local_time(), "Temp", 15.8).
%%M21 = pollution:addValue(M20, {120.1234, 10.5678}, calendar:local_time(), "PM10", 140.0).
%%M22 = pollution:addValue(M21, "Dietla", calendar:local_time(), "PM2.5", 120).
%%M23 = pollution:addValue(M22, "Dietla", calendar:local_time(), "Temp", 17).
%%
%%pollution:getStationMean(M23,"Bronowice","PM10").
%%pollution:getStationMean(M23,{50.2345, 18.3445},"PM2.5").
%%pollution:getStationMean(M23,"Azory","Temp").
%%
%%pollution:getDailyMean(M23,{2020,04,18},"PM10").
%%pollution:getDailyMean(M23,{2020,04,18},"PM2.5").
%%pollution:getDailyMean(M23,{2020,04,18},"Temp").
%%
%%pollution:getWorstDay(M23,"Bronowice","PM10").
%%pollution:getWorstDay(M23,"Azory","PM10").
%%pollution:getWorstDay(M23,"Dietla","PM10").
%%
%%pollution:getWorstHourlyStation(M23,{2020,04,18},14,"PM10").
%%pollution:getWorstHourlyStation(M23,{2020,04,18},14,"PM2.5").
%%pollution:getWorstHourlyStation(M23,{2020,04,18},14,"Temp")
%%
%%pollution:getStationMean(M23,"Azory","PM10").
%%M24 = pollution:addValue(M23, "Azory", calendar:local_time(), "PM10", 100).
%%M25 = pollution:removeValue(M24,"Azory",  calendar:local_time(), "PM10").
%%pollution:getStationMean(M25,"Azory","PM10").
%%
%%
%%
%%M26 = pollution:addStation(M25, "Bronowice", {31.23,45.67}).
%%M27 = pollution:addValue(M25, "Azory", calendar:local_time(), "PM10", 100).
%%M28 = pollution:addValue(M27, "Azory", calendar:local_time(), "PM10", 100).
%%M29 = pollution:addValue(M25, "Kryspinow", calendar:local_time(), "PM10", 100).