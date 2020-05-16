defmodule PollutionData do
  @moduledoc false

  def importLinesFromCSV(filename) do
    splitedLines = File.read!(filename)
                   |> String.split("\r\n")

    IO.puts("There are #{length(splitedLines)} lines")
    splitedLines
  end


  def convertLine(line) do
    [date, hour, len, width, value]=String.split(line,",")

    date = String.split(date,"-")
           |> Enum.reverse()
           |> Enum.map(fn x -> {val,_}=Integer.parse(x); val end)
           |> :erlang.list_to_tuple()


    hour = String.split(hour, ":")
           |> Enum.map(fn x -> {val,_}=Integer.parse(x); val end)
    hour = hour ++ [0]
           |> :erlang.list_to_tuple()


    coordinates = [len, width]
                  |> Enum.map(fn x -> {val,_}=Float.parse(x); val end)
                  |> :erlang.list_to_tuple()


    {val,_} = Integer.parse(value)

    %{:datetime=>{date,hour}, :location=>coordinates,:pollutionLevel=>val}

  end


  def identifyStations(data) do
    stations = data
               |> Enum.map(fn x -> x.location  end)
               |> Enum.uniq()
    IO.puts("There are #{length(stations)} uniq stations")
    stations
  end


  def loadStations(stations) do
    :pollution_sup.start_link()
    stations
    |> Enum.each(fn {x,y} -> "station_#{x}_#{y}"
                             |> :pollution_gen_server.addStation({x,y}) end )
  end


  def loadStationsData(data) do
    data
    |> Enum.each(fn x -> :pollution_gen_server.addValue(x.location, x.datetime, "PM10", x.pollutionLevel)end )
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
    |> Enum.map(&convertLine/1)

    stations = identifyStations(data)


    IO.puts("\nloadStations time #{measureTime(fn -> loadStations(stations) end) }" )
    IO.puts("loadStationsData time #{measureTime(fn -> loadStationsData(data) end) }")

    IO.puts("StationMean time #{measureTime(fn -> :pollution_gen_server.getStationMean({20.06, 49.986}, "PM10") end)}")
    IO.puts("DailyMean time #{measureTime(fn -> :pollution_gen_server.getDailyMean({2017, 5, 4}, "PM10") end)}")

  end

end
#    c("pollution.erl")
#    c("pollution_sup.erl")
#    c("pollution_gen_server.erl")