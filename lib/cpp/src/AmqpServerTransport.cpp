#include <assert.h>
#include <stdio.h>

#include <string>
#include <thrift/transport/TTransport.h>

#include "amqp.h"

#include "AmqpInterface.h"
#include "AmqpTransport.h"
#include "AmqpServerTransport.h"
#include "Debug.h"

using namespace waste;
using namespace apache::thrift::transport;
using namespace boost;
using namespace std;

const char* AmqpServerTransport::MODULE = "AmqpServerTransport";

// constructors and destructor
AmqpServerTransport::AmqpServerTransport(shared_ptr<Amqp::Channel> channel,
                                         const string& exchangeName,
                                         const string& routingKey,
                                         const string& queueName) :
    fChannel(channel),
    fExchangeName(exchangeName),
    fRoutingKey(routingKey),
    fQueueName(queueName)    
{
    assert(fChannel.get() != NULL);

    int result = fChannel->setupQueue(exchangeName, routingKey, queueName);
    result = result & fChannel->subscribe(queueName);

    // TODO : does something for exceptional cases    
}


AmqpServerTransport::~AmqpServerTransport()
{
    // nothing is done
}

// public member functions
void
AmqpServerTransport::close()
{
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "closing...");
    }
    // does nothing
}

// protected member functions
shared_ptr<TTransport>
AmqpServerTransport::acceptImpl()
{
    message_string message;
    string routingKey;
    string replyTo;

    fChannel->waitForMessage(message, replyTo);

    AmqpTransport* amqpTransport = new AmqpTransport(fChannel, fExchangeName, routingKey, replyTo);
    
    // write the message to the memory buffer    
    amqpTransport->write(message.data(), message.length());

    return ( shared_ptr<TTransport>(amqpTransport) );
}
