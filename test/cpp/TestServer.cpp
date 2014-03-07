#include <thrift/concurrency/ThreadManager.h>
#include <thrift/concurrency/PosixThreadFactory.h>
#include <thrift/protocol/TBinaryProtocol.h>
#include <thrift/server/TSimpleServer.h>
#include <thrift/server/TThreadPoolServer.h>
#include <thrift/server/TThreadedServer.h>
#include <thrift/transport/TTransportUtils.h>
#include <boost/shared_ptr.hpp>

#include <iostream>

#include "Amqp.h"
#include "AmqpFactory.h"
#include "AmqpServerTransport.h"

#include "Test.h"

using namespace std;
//using namespace boost;
using namespace apache::thrift;
using namespace apache::thrift::protocol;
using namespace apache::thrift::transport;
using namespace apache::thrift::server;

using namespace waste;
using namespace test;


class Test : public TestIf {
 public:
    Test() {}

    void echo(string& _return, const string& text) {

        _return.append("echo ");
        _return.append("\"");
        _return.append(text);        
        _return.append("\"");        
    }
};


int main(int argc, char **argv) {

    const string hostName = "127.0.0.1";
    const int port = 5672;
    const string virtualHostName = "/";
    const string userName = "guest";
    const string password = "guest";
    const string exchangeName = "test";
    const string routingKey = "test";
    const string queueName = "test";    

    boost::shared_ptr<Amqp> amqp = AmqpFactory::get(hostName, port, virtualHostName, userName, password);
    
    amqp->connect();
    boost::shared_ptr<Amqp::Channel> channel = amqp->open();
    
    boost::shared_ptr<TProtocolFactory> protocolFactory(new TBinaryProtocolFactory());
    boost::shared_ptr<Test> test(new Test());
    boost::shared_ptr<TProcessor> processor(new TestProcessor(test));
    boost::shared_ptr<TServerTransport> serverTransport(new AmqpServerTransport(channel, exchangeName, routingKey, queueName));
    boost::shared_ptr<TTransportFactory> transportFactory(new TFramedTransportFactory());

    TSimpleServer server(processor,
                         serverTransport,
                         transportFactory,
                         protocolFactory);

    /**
     * Or you could do one of these

     boost::shared_ptr<ThreadManager> threadManager =
     ThreadManager::newSimpleThreadManager(workerCount);
     boost::shared_ptr<PosixThreadFactory> threadFactory =
     boost::shared_ptr<PosixThreadFactory>(new PosixThreadFactory());
     threadManager->threadFactory(threadFactory);
     threadManager->start();
     TThreadPoolServer server(processor,
     serverTransport,
     transportFactory,
     protocolFactory,
     threadManager);

     TThreadedServer server(processor,
     serverTransport,
     transportFactory,
     protocolFactory);

    */

    cout << "Starting the server..." << endl;
    server.serve();
    cout << "done." << endl;

    channel->close();
    amqp->disconnect();
    return 0;
}
