%% amqp server template
%%
%% A module that implements this template will also have to
%% implement gen_server template. "


-module(waste_amqp).

-export([behaviour_info/1]).

-export([new/2,
         connect/1,
         open/1,
         disconnect/1
        ]).

behaviour_info(callbacks) ->
    [{connect, 1},
     {open, 1},
     {disconnect, 1}
    ].

-record(amqp, {module, data}).

new(Module, Data) when is_atom(Module) ->
    {ok, #amqp{module = Module,
               data = Data}}.

%% Data :: iolist()
connect(#amqp{module = Module, data = Data}) ->
    Module:connect(Data).

open(#amqp{module = Module, data = Data}) ->
    Module:open(Data).

disconnect(#amqp{module = Module, data = Data}) ->
    Module:disconnect(Data).
