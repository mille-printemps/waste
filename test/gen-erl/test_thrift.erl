%%
%% Autogenerated by Thrift
%%
%% DO NOT EDIT UNLESS YOU ARE SURE THAT YOU KNOW WHAT YOU ARE DOING
%%

-module(test_thrift).
-behaviour(thrift_service).


-include("test_thrift.hrl").

-export([struct_info/1, function_info/2]).

struct_info('i am a dummy struct') -> undefined.
%%% interface
% echo(This, Text)
function_info('echo', params_type) ->
  {struct, [{1, string}]}
;
function_info('echo', reply_type) ->
  string;
function_info('echo', exceptions) ->
  {struct, []}
;
function_info(xxx, dummy) -> dummy.
