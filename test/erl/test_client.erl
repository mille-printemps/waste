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
    ok = waste_amqp:connect(Amqp),
    {ok, Channel} = waste_amqp:open(Amqp),

    {ok, AmqpTransport} = waste_amqp_transport:new(Channel, X, RoutingKey),
    {ok, ProtocolFactory} =
        thrift_binary_protocol:new_protocol_factory(
          fun() -> thrift_framed_transport:new(AmqpTransport) end, []),

    %% execution
    {ok, Client} = thrift_client:start_link(ProtocolFactory, test_thrift),

    Request = "hello",
    io:format("send -> ~s~n", [Request]),
    {ok, Result} = thrift_client:call(Client, echo, [Request]),
    io:format("receive -> ~s~n", [erlang:binary_to_list(Result)]),

    ok = thrift_client:close(Client),

    %% post-process
    ok = waste_channel:close(Channel),
    ok = waste_amqp:disconnect(Amqp).
