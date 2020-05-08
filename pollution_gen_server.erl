%%%-------------------------------------------------------------------
%%% @author Natalia Brzozowska
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Natalia Brzozowska").

%% API
-export([start/0, stop/0, crash/0, addStation/2, addValue/4, removeValue/3, getDailyMean/2, getOneValue/3, getStationMean/2, getWorstDay/2, getWorstHourlyStation/3, handle_cast/2]).
-export([init/1, handle_call/3, check_error/2,terminate/2]).
-behaviour(gen_server).


%% START
start() ->
  M=pollution:createMonitor(),
  gen_server:start_link({local,pollution_gen_server},pollution_gen_server,M,[]).

init(M) ->
  {ok,M}.

%% CLIENT
crash() ->gen_server:cast(pollution_gen_server,crash).
addStation(Name, Coordinates) -> gen_server:call(pollution_gen_server,{addStation,[Name,Coordinates]}).
addValue(Id, Date, Type, Value)-> gen_server:call(pollution_gen_server,{addValue,[Id,Date, Type, Value]}).
removeValue(Id, Date, Type)-> gen_server:call(pollution_gen_server,{removeValue,[Id, Date, Type]}).
getOneValue(Id, Date, Type)-> gen_server:call(pollution_gen_server,{getOneValue,[Id, Date, Type]}).
getStationMean(Id, Type)-> gen_server:call(pollution_gen_server,{getStationMean,[Id, Type]}).
getDailyMean(Day,Type)-> gen_server:call(pollution_gen_server,{getDailyMean, [Day, Type]}).
getWorstDay(Id, Type)-> gen_server:call(pollution_gen_server,{getWorstDay,[Id, Type]}).
getWorstHourlyStation(Day, Hour, Type)-> gen_server:call(pollution_gen_server,{getWorstHourlyStation,[Day, Hour, Type]}).
stop() -> gen_server:call(pollution_gen_server,terminate).

%% SERVER

check_error(error,Msg) ->io:format("received error -  ~s ~n",[Msg]);
check_error(reply,_)->ok.

handle_call({addStation,[Name,Coordinates]},_From, M) ->
  {Atom, Msg,M2} = pollution:addStation(M,Name,Coordinates),
  check_error(Atom,Msg),
  {reply,Msg,M2};
handle_call({addValue,[Id,Date, Type, Value]}, _From,M) ->
  {Atom, Msg, M2}=pollution:addValue(M,Id,Date, Type,Value),
  check_error(Atom,Msg),
  {reply,Msg,M2};
handle_call({removeValue,[Id, Date, Type]},_From, M) ->
  {Atom, Msg, M2}=pollution:removeValue(M,Id,Date, Type),
  check_error(Atom,Msg),
  {reply,Msg,M2};
handle_call({getOneValue,[Id, Date, Type]}, _From, M) ->
  {Atom,Val}=pollution:getOneValue(M,Id,Date,Type),
  check_error(Atom,Val),
  {reply,Val,M};
handle_call({getStationMean,[Id, Type]}, _From, M) ->
  {Atom,Val}=pollution:getStationMean(M,Id,Type),
  check_error(Atom,Val),
  {reply,Val,M};
handle_call({getDailyMean, [Day, Type]}, _From, M) ->
  {Atom,Val}=pollution:getDailyMean(M,Day,Type),
  check_error(Atom,Val),
  {reply,Val,M};
handle_call({getWorstDay,[Id, Type]}, _From, M) ->
  {Atom,Val}=pollution:getWorstDay(M,Id,Type),
  check_error(Atom,Val),
  {reply,Val,M};
handle_call({getWorstHourlyStation,[Day, Hour, Type]}, _From, M) ->
  {Atom,Val}=pollution:getWorstHourlyStation(M,Day, Hour,Type),
  check_error(Atom,Val),
  {reply,Val,M};
handle_call(terminate, _From, M) ->
  {stop,normal,stopped,M}.

terminate(normal,_) -> io:format("Closing monitor. Reason is normal ~n"),ok;
terminate(other,_) -> io:format("Closing monitor. Reason is other ~n"),ok;
terminate(kill,_) -> io:format("Closing monitor. Reason is killed ~n"),ok.



handle_cast(crash, M) ->
  pollution:haveFun(),
  {noreply,M}.