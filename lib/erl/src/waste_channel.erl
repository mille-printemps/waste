%% channel server template
%%
%% the same thing as amqp server template is applicable to this.

-module(waste_channel).

-export([behaviour_info/1]).

-export([new/2,
         setup_queue/4,
         subscribe/3,
         publish/5,
         close/1
        ]).

behaviour_info(callbacks) ->
    [{setup_queue, 4},
     {subscribe, 3},
     {publish, 5},
     {close, 1}
    ].

-record(channel, {module, data}).

new(Module, Data) when is_atom(Module) ->
    {ok, #channel{module = Module,
                  data = Data}}.

setup_queue(Channel, X, RoutingKey, Q) ->
    Module = Channel#channel.module,
    Module:setup_queue(Channel#channel.data, X, RoutingKey, Q).

subscribe(Channel, Q, Consumer) ->
    Module = Channel#channel.module,
    Module:subscribe(Channel#channel.data, Q, Consumer).

publish(Channel, X, RoutingKey, Message, ReplyTo) ->
    Module = Channel#channel.module,
    Module:publish(Channel#channel.data, X, RoutingKey, Message, ReplyTo).

close(#channel{module = Module, data = Data}) ->
    Module:close(Data).


