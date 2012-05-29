-module(waste_amqp_transport).

-include_lib("lib/rabbit_common/include/rabbit_framing.hrl").

-behaviour(gen_server).
-behaviour(thrift_transport).

%% API
-export([new/3,
         new_transport_factory/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% thrift_transport callbacks
-export([write/2,
         read/2,
         flush/1,
         close/1]).

-record(amqp_transport, {channel,
                         exchange,
                         routing_key,
                         reply_to,
                         buffer,
                         waiting_for_message}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------

new(Channel, X, RoutingKey) ->
    case gen_server:start_link(?MODULE, [Channel, X, RoutingKey], []) of
        {ok, Pid} ->
            thrift_transport:new(?MODULE, Pid);
        Else ->
            Else
    end.

new_transport_factory(Channel, X, RoutingKey) ->
    {ok, fun() -> new(Channel, X, RoutingKey) end}.

%%--------------------------------------------------------------------
%% Function: write(Transport, Data) -> ok
%%
%% Data = iolist()
%%
%% Description: Writes data into the buffer
%%--------------------------------------------------------------------
write(Transport, Data) ->
    gen_server:call(Transport, {write, Data}).

%%--------------------------------------------------------------------
%% Function: flush(Transport) -> ok
%%
%% Description: Flushes the buffer through to the wrapped transport
%%--------------------------------------------------------------------
flush(Transport) ->
    gen_server:call(Transport, flush).

%%--------------------------------------------------------------------
%% Function: close(Transport) -> ok
%%
%% Description: Closes the transport and the wrapped transport
%%--------------------------------------------------------------------
close(Transport) ->
    gen_server:cast(Transport, close).

%%--------------------------------------------------------------------
%% Function: Read(Transport, Len) -> {ok, Data}
%%
%% Data = binary()
%%
%% Description: Reads data through from the wrapped transoprt
%%--------------------------------------------------------------------
read(Transport, Len) when is_integer(Len) ->
    gen_server:call(Transport, {read, Len}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%%                         {ok, State, Timeout} |
%%                         ignore               |
%%                         {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Channel, X, RoutingKey]) ->
    %% creates a reply queue name
    ReplyTo = uuid(),

    %% bind the queue with the exchange and the routing key
    waste_channel:setup_queue(Channel, X, ReplyTo, ReplyTo),

    %% subscribes the queue
    waste_channel:subscribe(Channel, ReplyTo, self()),
    
    {ok, #amqp_transport{channel = Channel,
                         exchange = X,
                         routing_key = RoutingKey,
                         buffer = [],
                         reply_to = ReplyTo,
                         waiting_for_message = true}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%%                                      {reply, Reply, State, Timeout} |
%%                                      {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, Reply, State} |
%%                                      {stop, Reason, State}
%% Description: Handling call buffers
%%--------------------------------------------------------------------
handle_call({write, Data}, _From, State = #amqp_transport{buffer = Buffer}) ->
    {ok, NewState} = write_buffer(Buffer, Data, State),
    {reply, ok, NewState};
    

handle_call({read, Len}, _From, State = #amqp_transport{buffer = Buffer, waiting_for_message = WaitingForMessage}) ->
    %% TODO : treat timeout cases
    case WaitingForMessage of
        true ->
            receive
                {#'basic.deliver'{}, Content} ->
%                    {content, _ClassId, _Properties, _PropertiesBin, [Payload]} = Content,
                    {amqp_msg, _Properties, Payload} = Content,
                    {ok, WriteState} = write_buffer(Buffer, Payload, State),
                    {Result, ReadState} = read_buffer(WriteState#amqp_transport.buffer, Len, WriteState),
                    {reply, Result, ReadState};
                
                Other ->
                    error_logger:error_report({?MODULE, ?LINE, [Other], erlang:get_stacktrace()})
            end;
        false ->
            {Result, ReadState} = read_buffer(Buffer, Len, State),
            {reply, Result, ReadState}
    end;


handle_call(flush, _From, State = #amqp_transport{channel = Channel,
                                                  exchange = X,
                                                  routing_key = RoutingKey,
                                                  buffer = Buffer,
                                                  reply_to = ReplyTo}) ->
    waste_channel:publish(Channel, X, RoutingKey,
                    iolist_to_binary(Buffer), iolist_to_binary(ReplyTo)),
    {reply, ok, State#amqp_transport{buffer = [], waiting_for_message = true}}.


%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%%                                      {noreply, State, Timeout} |
%%                                      {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(close, State = #amqp_transport{channel = _Channel}) ->
    {stop, normal, State};

handle_cast(_Message, State = #amqp_transport{}) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%%                                       {noreply, State, Timeout} |
%%                                       {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(#'basic.consume_ok'{}, State) ->
    {noreply, State};

handle_info(#'basic.cancel_ok'{}, State) ->
    {stop, normal, State};

handle_info({#'basic.deliver'{}, Content}, State) ->
    Pid = self(),
    Pid ! {#'basic.deliver'{}, Content},
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

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

write_buffer(Buffer, Data, State) ->
    BufferState = State#amqp_transport{buffer = [Buffer, Data]},
    MessageState = BufferState#amqp_transport{waiting_for_message = false},
    {ok, MessageState}.

read_buffer(Buffer, Len, State) ->
    Binary = iolist_to_binary(Buffer),
    Give = erlang:min(iolist_size(Binary), Len),
    {Result, Remaining} = split_binary(Binary, Give),
    {{ok, Result}, State#amqp_transport{buffer = Remaining}}.
    
%%min(A,B) when A<B -> A;
%%min(_,B)          -> B.

uuid() ->
    {Year, Month, Day} = erlang:date(),
    {_MegaSec, Sec, MicroSec} = erlang:now(),
    ID = io_lib:format("~4.10.0B~2.10.0B~2.10.0BT~.10.0B.~.10B", [Year, Month, Day, Sec, MicroSec]),
    erlang:iolist_to_binary(ID).

    
    
