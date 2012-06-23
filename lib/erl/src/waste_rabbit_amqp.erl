%% implementation of amqp server template

-module(waste_rabbit_amqp).

%%-include_lib("lib/rabbit_common/include/rabbit_framing.hrl").
%%-include_lib("lib/amqp_client/include/amqp_client.hrl").
-include("amqp_client.hrl").

-behaviour(gen_server).
-behaviour(waste_amqp).
%%-behaviour(amqp).

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

-record(state, {serverhost,
                port,
                vhostpath,
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

new(Host, Port, VHost, User, Password) ->
    case gen_server:start_link(?MODULE, [Host, Port, VHost, User, Password], []) of
        {ok, Pid} ->
            waste_amqp:new(?MODULE, Pid);
        Else ->
            Else
    end.


new_amqp_factory(Host, Port, VHost, User, Password) ->
    {ok, fun() -> new(Host, Port, VHost, User, Password) end}.


connect(AmqpPid) ->
    gen_server:call(AmqpPid, connect).

open(AmqpPid) ->
    gen_server:call(AmqpPid, open).

disconnect(AmqpPid) ->
    gen_server:call(AmqpPid, disconnect).


%%====================================================================
%% gen_server callbacks
%%====================================================================

init([Host, Port, VHost, User, Password]) ->
    {ok, #state{serverhost = Host,
                port = Port,
                vhostpath = VHost,
                username = User,
                password = Password}}.


handle_call(connect, _From, State = #state{serverhost = Host,
                                           port = _Port,
                                           vhostpath = VHost,
                                           username = User,
                                           password = Password}) ->
%%    Connection = amqp_connection:start(User, Password, Host, VHost),
    AmqpParams = #'amqp_params_network'{username = User,
                                        password = Password,
                                        host = Host,
                                        virtual_host = VHost},
    %%Connection = amqp_connection:start(AmqpParams),
    {ok, Connection} = amqp_connection:start(AmqpParams),        
    {reply, ok, State#state{connection = Connection}};


handle_call(open, _From, State = #state{connection = Connection}) ->
%%    Parent = amqp_connection:open_channel(Connection),
    {ok, Parent} = amqp_connection:open_channel(Connection),    
    {ok, Channel} = waste_rabbit_channel:new(Parent),
    {reply, {ok, Channel}, State};


handle_call(disconnect, _From, State = #state{connection = Connection}) ->
%    ConnectionClose = #'connection.close'{reply_code = 200,
%                                          reply_text = <<"Goodbye">>,
%                                          class_id = 0,
%                                          method_id = 0},
%    #'connection.close_ok'{} = amqp_connection:close(Connection, ConnectionClose),
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


    
    
    
    

