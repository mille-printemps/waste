%% poolboy worker of waste client

-module(waste_client_worker).

-behaviour(gen_server).
-behaviour(poolboy_worker).

-export([start_link/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {channel,
                exchange,
                routing_key,
                amqp,
                client}).

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

init(Args) ->
    process_flag(trap_exit, true),
    ServerHost = proplists:get_value(server_host, Args),
    Port = proplists:get_value(port, Args),
    VHostPath = proplists:get_value(virtual_host_path, Args),
    Username = proplists:get_value(username, Args),
    Password = proplists:get_value(password, Args),
    Exchange = proplists:get_value(exchange, Args),
    RoutingKey = proplists:get_value(routing_key, Args),
    ServiceName = proplists:get_value(service_name, Args),
    
    {ok, Amqp} = waste_rabbit_amqp:new(ServerHost, Port, VHostPath, Username, Password),
    waste_amqp:connect(Amqp),
    {ok, Channel} = waste_amqp:open(Amqp),
    
    {ok, AmqpTransport} = waste_amqp_transport:new(Channel, Exchange, RoutingKey),
    {ok, ProtocolFactory} =
        thrift_binary_protocol:new_protocol_factory(
          fun() -> thrift_framed_transport:new(AmqpTransport) end, []),
    {ok, Protocol} = ProtocolFactory(),
    {ok, Client} = thrift_client:new(Protocol, ServiceName),    

    {ok, #state{channel=Channel,
                exchange=Exchange,
                routing_key=RoutingKey,
                amqp=Amqp,
                client=Client}}.


handle_call({call, Function, Args}, _From, State = #state{client = Client}) ->
    {reply, thrift_client:call(Client, Function, Args), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{channel = Channel,
                          amqp = Amqp,
                          client = Client}) ->
    thrift_client:close(Client),
    waste_channel:close(Channel),
    waste_amqp:disconnect(Amqp),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


