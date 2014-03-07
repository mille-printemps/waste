%% An example of RPC client

-module(test_client).

-export([test/0]).

test() ->
    Host = "127.0.0.1",
    Port = 5672,
    VHost = <<"/">>,
    User = <<"guest">>,
    Password = <<"guest">>,
    X = <<"test">>,
    RoutingKey = <<"test">>,

    %% pre-process
    {ok, Amqp} = waste_rabbit_amqp:new(Host, Port, VHost, User, Password),
    waste_amqp:connect(Amqp),
    {ok, Channel} = waste_amqp:open(Amqp),

    {ok, AmqpTransport} = waste_amqp_transport:new(Channel, X, RoutingKey),
    {ok, ProtocolFactory} =
        thrift_binary_protocol:new_protocol_factory(
          fun() -> thrift_framed_transport:new(AmqpTransport) end, []),
    {ok, Protocol} = ProtocolFactory(),

    %% execution
    {ok, Client} = thrift_client:new(Protocol, test_thrift),    

    Request = "hello",
    io:format("send -> ~s~n", [Request]),
    {_, {ok, Result}} = thrift_client:call(Client, echo, [Request]),    
    io:format("receive -> ~s~n", [erlang:binary_to_list(Result)]),

    thrift_client:close(Client),        

    %% post-process
    waste_channel:close(Channel),
    waste_amqp:disconnect(Amqp).
