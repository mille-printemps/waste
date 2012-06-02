// File: TestClient.cpp - last edit:
// Chiharu Kawatake	05-30-2012
// Copyright (c) 2012 by Chiharu Kawatake
// All rights reserved.

#include <thrift/Thrift.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/transport/TTransportUtils.h>

#include <iostream>

#include "Amqp.h"
#include "AmqpFactory.h"
#include "AmqpTransport.h"

#include "Test.h"

using namespace std;
using namespace boost;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;

using namespace waste;
using namespace test;

int main(int argc, char** argv) {
    const string hostName = "127.0.0.1";
    const int port = 5672;
    const string virtualHostName = "/";
    const string userName = "guest";
    const string password = "guest";
    const string exchangeName = "test";
    const string routingKey = "test";
    const string queueName = "";    
    
    shared_ptr<Amqp> amqp = AmqpFactory::get(hostName, port, virtualHostName, userName, password);
    
    amqp->connect();
    shared_ptr<Amqp::Channel> channel = amqp->open();
    
    shared_ptr<TTransport> amqpTransport(new AmqpTransport(channel, exchangeName, routingKey, queueName));
    shared_ptr<TTransport> transport(new TFramedTransport(amqpTransport));

    shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
    TestClient client(protocol);

    try {
        transport->open();

        string request = "hello";
        string response;
        
        cout << "send -> " << request << endl;
        client.echo(response, request);
        cout << "receive -> " << response << endl;

        transport->close();
    }
    catch (TException &tx) {
        cout << "ERROR: " << tx.what() << endl;
    }

    channel->close();
    amqp->disconnect();
}

// LOG:
// 07-02-2009 Chiharu Kawatake	created
