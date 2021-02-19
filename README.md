Pollution monitoring
=====

An OTP application made for Erlang and Elixir classes at AGH University.  

Description
---  

Pollution monitoring is a rebar3 project, which uses the OTP *application* pattern.  
[Version](https://github.com/brzozia/rebar_pollution_with_mnesia) with Mnesia database.   
#

Module *src/pollution* is an Erlang module whose task is to process and store data about air pollution. It also enables to store and process data about measuring stations.  

Stored data about stations are:  
* station name
* geographical coordinates

Stored measurements data contains:  
* geographical coordinates
* date and hour of measurement
* value
* value type (eg. PM10, PM2.5, temperature)

Module gives functions to operate on data.

#
*src/pollution_gen_server* is a server for pollution module. 

#
Elixir modules are used for loading data from csv to server. There are two types of loading data - one of them uses *stream* and other one not.

#
Common tests are included in the code.

#

Build
----

    $ rebar3 compile
