# Waste - Experimental RPC library using Thrift and AMQP

## Introduction

Waste is an experimental library for RPC using Thrift and AMQP.
This is named so just because "waste" is an antonym of "thrift".
 
Motivation behind Waste is to utilize services in 
a fault-tolerant way from clients that are implemented
in different programming languages.

Waste extends a transport layer of Thrift so that it can
put requests from clients to a queue and dispatch the requests to
a service. If multiple services (on multiple physical servers)
are subscribing the queue, the load of the requests is balanced
among the servers. 


## How to build

### Install Erlang

Erlang runtime library and emulator can be downloaded at

- <http://www.erlang.org/download.html>

Version R14A or later will be needed. Install it as instructed
in the package.

### Install Thrift

Thrift can be downloaded at

- <http://archive.apache.org/dist/thrift/>

Note that curretly Waste supports Thrift Version 0.9.0 or later.
Install it as instructed in the package.


### Install RabbitMQ C library

RabbitMQ C library can be downloaded at

- <https://github.com/alanxz/rabbitmq-c>

Install it as instructed.

### Building the code

Run the following commands.

    export PATH=$PATH:`pwd` # add the current working directory to the path
    ./bootstrap.sh
    ./configure
    make

Before typing 'make', make sure that 'rebar', which is used to compile poolboy, is on your PATH environment variable. 

## Usage

Currently, an example client can be executed int the following way. 

* Go to deps/rabbitmq-server/ directory and type "make run". This will start RabbitMQ.
* Go to test/cpp and type "./TestServer". This will start the test server.
* Go to test/erl and type "./client.sh". This will send a request to the server and receive a response.

If everything goes well, the following will be displayed.

    %> ./client.sh
    send -> hello
    receive -> echo "hello"

    =INFO REPORT==== 10-Mar-2014::23:41:08 ===
    application: waste_client
    exited: stopped
    type: temporary

'test_client' is implemented as an application of 'waste_client', which uses poolboy. The parameters of the application can be modified in waste_client.app, which looks like below. 

    {application, waste_client, [
        {description, "A waste client application"},
        {vsn, "0.1"},
        {applications, [kernel, stdlib]},
        {modules, [waste_client, waste_client_worker]},
        {registered, [waste_client]},
        {mod, {waste_client, []}},
        {env, [
            {pools, [
                {test_pool, [
                    {size, 10},
                    {max_overflow, 20}
                ], [
                    {server_host, "127.0.0.1"},
                    {port, 5672},
                    {virtual_host_path, <<"/">>},
                    {username, <<"guest">>},
                    {password, <<"guest">>},
                    {exchange, <<"test">>},
                    {routing_key, <<"test">>},
                    {service_name, test_thrift }                
                ]}
            ]}
        ]}
    ]}.

## Meta

Written by Chiharu Kawatake

Released under the MIT License: http://www.opensource.org/licenses/mit-license.php

This application is provided 'AS IS' and the user assumes all risks associated with its use.
