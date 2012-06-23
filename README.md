# Waste - Experimental fault-tolerant RPC library using Thrift and AMQP

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

- <http://archive.apache.org/dist/incubator/thrift/>

Note that curretly Waste only supports Thrift Version 0.4.0.
Install it as instructed in the package.


### Install RabbitMQ C library

RabbitMQ C library can be downloaded at

- <https://github.com/alanxz/rabbitmq-c>

Install it as instructed.

### Building the code

Run the following commands.

    ./bootstrap.sh
    ./configure
    make


## Usage

Currently, an example client can be executed int the following way. 

* Go to deps/rabbitmq-server/ directory and type "make run". This will start RabbitMQ.
* Go to test/cpp and type "./TestServer". This will start the test server.
* Go to test/erl and type "./client.sh". This will send a request to the server and receive a response.

If everything goes well, the following will be displayed.

    %> ./client.sh
    send -> hello
    receive -> echo "hello"

## Meta

Written by Chiharu Kawatake

Released under the MIT License: http://www.opensource.org/licenses/mit-license.php

This application is provided 'AS IS' and the user assumes all risks associated with its use.
