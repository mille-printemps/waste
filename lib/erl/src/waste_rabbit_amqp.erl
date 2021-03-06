%% implementation of amqp server template

-module(waste_rabbit_amqp).

-include("amqp_client.hrl").

-behaviour(gen_server).
-behaviour(waste_amqp).

%% API
-export([new/5,
         new_amqp_factory/5]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% amqp callbacks
-export([connect/1,
         open/1,
         disconnect/1]).

-record(state, {server_host,
                port,
                virtual_host_path,
                username,
                password,
                connection}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
new(ServerHost, Port, VHostPath, Username, Password) ->
    case gen_server:start_link(?MODULE, [ServerHost, Port, VHostPath, Username, Password], []) of
        {ok, Pid} ->
            waste_amqp:new(?MODULE, Pid);
        Else ->
            Else
    end.

new_amqp_factory(ServerHost, Port, VHostPath, Username, Password) ->
    {ok, fun() -> new(ServerHost, Port, VHostPath, Username, Password) end}.


connect(AmqpPid) ->
    gen_server:call(AmqpPid, connect).

open(AmqpPid) ->
    gen_server:call(AmqpPid, open).

disconnect(AmqpPid) ->
    gen_server:call(AmqpPid, disconnect).


%%====================================================================
%% gen_server callbacks
%%====================================================================
init([ServerHost, Port, VHostPath, Username, Password]) ->
    {ok, #state{server_host = ServerHost,
                port = Port,
                virtual_host_path = VHostPath,
                username = Username,
                password = Password}}.


handle_call(connect, _From, State = #state{server_host = ServerHost,
                                           port = _Port,
                                           virtual_host_path = VHostPath,
                                           username = Username,
                                           password = Password}) ->
    AmqpParams = #'amqp_params_network'{username = Username,
                                        password = Password,
                                        host = ServerHost,
                                        virtual_host = VHostPath},
    {ok, Connection} = amqp_connection:start(AmqpParams),        
    {reply, ok, State#state{connection = Connection}};


handle_call(open, _From, State = #state{connection = Connection}) ->
    {ok, Parent} = amqp_connection:open_channel(Connection),    
    {ok, Channel} = waste_rabbit_channel:new(Parent),
    {reply, {ok, Channel}, State};


handle_call(disconnect, _From, State = #state{connection = Connection}) ->
    amqp_connection:close(Connection),
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
