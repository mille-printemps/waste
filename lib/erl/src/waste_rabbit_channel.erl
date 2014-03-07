%% implementation of channel server template

-module(waste_rabbit_channel).

-include("amqp_client.hrl").

-behaviour(gen_server).
-behaviour(waste_channel).


%% API
-export([new/1,
         new_channel_factory/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% channel callbacks
-export([setup_queue/4,
         subscribe/3,
         publish/5,
         close/1]).

-record(state, {channel}).


%%====================================================================
%% API
%%====================================================================
new(Channel) ->
    case gen_server:start_link(?MODULE, [Channel], []) of
        {ok, Pid} ->
            waste_channel:new(?MODULE, Pid);
        Else ->
            Else
    end.


new_channel_factory(Channel) ->
    {ok, fun() -> new(Channel) end}.


setup_queue(ChannelPid, X, RoutingKey, Q) ->
    gen_server:call(ChannelPid, {setup_queue, X, RoutingKey, Q}).


subscribe(ChannelPid, Q, Consumer) ->
    gen_server:call(ChannelPid, {subscribe, Q, Consumer}).


publish(ChannelPid, X, RoutingKey, Message, ReplyTo) ->
    gen_server:call(ChannelPid, {publish, X, RoutingKey, Message, ReplyTo}).


close(ChannelPid) ->
    gen_server:call(ChannelPid, close).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([Channel]) ->
    {ok, #state{channel = Channel}}.


handle_call({setup_queue, X, RoutingKey, Q}, _From, State = #state{channel = Channel}) ->
    %% declare exchange
    ExchangeDeclare = #'exchange.declare'{ticket = 0,
                                         exchange = X,
                                         type = <<"direct">>,
                                         passive = false,
                                         durable = false,
                                         auto_delete = false,
                                         internal = false,
                                         nowait = false,
                                         arguments = []},
    amqp_channel:call(Channel, ExchangeDeclare),

    %% declare queue
    QueueDeclare = #'queue.declare'{ticket = 0,
                                    queue = Q,
                                    passive = false,
                                    durable = false,
                                    exclusive = false,
                                    auto_delete = false,
                                    nowait = false,
                                    arguments = []},
    amqp_channel:call(Channel, QueueDeclare),

    %% bind them    
    QueueBind = #'queue.bind'{ticket = 0,
                              queue = Q,
                              exchange = X,
                              routing_key = RoutingKey,
                              nowait = false,
                              arguments = []},
    amqp_channel:call(Channel, QueueBind),
    
    {reply, ok, State};


handle_call({subscribe, Q, Consumer}, _From, State = #state{channel = Channel}) ->
    %% consume queue
    BasicConsume = #'basic.consume'{queue = Q,
                                    consumer_tag = <<>>,
                                    no_local = false,
                                    no_ack = true,
                                    exclusive = false,
                                    nowait = false},

    amqp_channel:subscribe(Channel, BasicConsume, Consumer),

    {reply, ok, State};

    
handle_call({publish, X, RoutingKey, Message, ReplyTo}, _From, State = #state{channel = Channel}) ->
    %% publish message
    Properties = #'P_basic'{reply_to = ReplyTo},
    BasicPublish = #'basic.publish'{exchange = X,
                                    routing_key = RoutingKey,
                                    mandatory = false,
                                    immediate = false},
    Content = #amqp_msg{props = Properties, payload = Message},
    amqp_channel:cast(Channel, BasicPublish, Content),
    {reply, ok, State};


handle_call(close, _From, State = #state{channel = Channel}) ->
    amqp_channel:close(Channel),    

    {reply, ok, State}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Message, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Message, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.


%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

