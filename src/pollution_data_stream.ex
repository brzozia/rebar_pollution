defmodule PollutionDataStream do
  @moduledoc false

  def importLinesFromCSV(filename) do
    splitedLines = File.stream!(filename)
                   |> Enum.take(5904)

    IO.puts("There are #{length(splitedLines)} lines")
    splitedLines
  end


  def convertLine(line) do
    [date, hour, len, width, value]=String.split(line,",")

    date = String.split(date,"-")
           |> Enum.reverse()
           |> Stream.map(fn x -> {val,_}=Integer.parse(x); val end)
           |> Enum.map(fn x -> x end)
           |> :erlang.list_to_tuple()


    hour = String.split(hour, ":")
           |> Stream.map(fn x -> {val,_}=Integer.parse(x); val end)
           |> Enum.map(fn x -> x end)
    hour = hour ++ [0]
           |> :erlang.list_to_tuple()


    coordinates = [len, width]
                  |> Stream.map(fn x -> {val,_} = Float.parse(x); val end)
                  |> Enum.map(fn x -> x end)
                  |> :erlang.list_to_tuple()

    {val,_} = Integer.parse(value)

    %{:datetime=>{date,hour}, :location=>coordinates,:pollutionLevel=>val}

  end


  def identifyStations(data) do
    stations = data
               |> Stream.map(fn x -> x.location  end)
               |> Stream.uniq()
               |> Enum.map(fn x->x  end)
    IO.puts("There are #{length(stations)} uniq stations")
    stations
  end


  def loadStations(stations) do
    :pollution_sup.start_link()

    stations
     |> Stream.map(fn {x,y} -> ["station_#{x}_#{y}",{x,y}] end)
     |> Enum.each(fn [name, coords] -> :pollution_gen_server.addStation(name,coords) end)

  end


  def loadStationsData(data) do
    data
    |> Stream.map(fn x -> {x.location, x.datetime, "PM10", x.pollutionLevel} end)
    |> Enum.each(fn {loc,time,type,val} -> :pollution_gen_server.addValue(loc,time,type,val)end )

  end


  def measureTime(fun) do
  fun
  |> :timer.tc
  |> elem(0)
  |> Kernel./(1_000_000)
  end


  def importData(filename) do
    data =
      filename
      |> importLinesFromCSV()
      |> Stream.map(&convertLine/1)
      |> Enum.map(fn x-> x end)

    stations = identifyStations(data)

    IO.puts("\nloadStations time #{measureTime(fn -> loadStations(stations) end) }" )
    IO.puts("loadStationsData time #{measureTime(fn -> loadStationsData(data) end) }")
    :timer.sleep(200);
    IO.puts("StationMean time #{measureTime(fn -> :pollution_gen_server.getStationMean({20.06, 49.986}, "PM10") end)}")
    IO.puts("DailyMean time #{measureTime(fn -> :pollution_gen_server.getDailyMean({2017, 5, 3}, "PM10") end)}")

  end

end

#    c("pollution.erl")
#    c("pollution_sup.erl")
#    c("pollution_gen_server.erl")