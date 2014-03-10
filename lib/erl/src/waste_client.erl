-module(waste_client).

-behaviour(application).
-behaviour(supervisor).

-export([start/0, stop/0, call/3]).
-export([start/2, stop/1]).
-export([init/1]).

start() ->
    application:start(?MODULE).    

stop() ->
    application:stop(?MODULE).

start(_Type, _Args) ->
    supervisor:start_link({local, waste_client_sup}, ?MODULE, []).

stop(_State) ->
    ok.

init(_Args) ->
    {ok, Pools} = application:get_env(waste_client, pools),
    PoolSpecs = lists:map(fun({Name, SizeArgs, WorkerArgs}) ->
        PoolArgs = [{name, {local, Name}},
                    {worker_module, waste_client_worker}] ++ SizeArgs,
        poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),
    {ok, {{one_for_one, 10, 10}, PoolSpecs}}.

call(PoolName, Function, Args) ->
    poolboy:transaction(PoolName, fun(Worker) ->
        gen_server:call(Worker, {call, Function, Args})
    end).

