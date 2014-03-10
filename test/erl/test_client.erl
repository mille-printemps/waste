%% An example of RPC client

-module(test_client).

-export([test/0]).

test() ->
    waste_client:start(),
    
    Request = "hello",
    io:format("send -> ~s~n", [Request]),
    {_, {ok, Result}} = waste_client:call(test_pool, echo, [Request]),    
    io:format("receive -> ~s~n", [erlang:binary_to_list(Result)]),

    waste_client:stop().
