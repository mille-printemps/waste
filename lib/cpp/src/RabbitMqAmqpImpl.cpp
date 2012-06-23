#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <assert.h>

#include <iostream>
#include <string>
#include <limits>

#include "amqp.h"
#include "amqp_framing.h"

#include "RabbitMqAmqpImpl.h"
#include "Debug.h"

using namespace waste;
using namespace boost;
using namespace std;

const int RabbitMqAmqpImpl::CHANNEL_MAX = 0;
const int RabbitMqAmqpImpl::FRAME_MAX = 131072;
const int RabbitMqAmqpImpl::DEFAULT_CHANNEL = 1;
const string RabbitMqAmqpImpl::DEFAULT_VIRTUAL_HOST_NAME = "/";

const char* RabbitMqAmqpImpl::MODULE = "RabbitMqAmqpImpl";
const uint16_t RabbitMqAmqpImpl::INITIAL_CHANNEL = 0;

const char* RabbitMqChannelImpl::MODULE = "RabbitMqChannelImpl";


// RabbitMqAmqpImpl
// constructors and destructor
RabbitMqAmqpImpl::RabbitMqAmqpImpl(const string& hostName,
                                   const int port,
                                   const string& virtualHostName,
                                   const string& userName,
                                   const string& password):
    fHostName(hostName),
    fPort(port),
    fVirtualHostName(virtualHostName),
    fUserName(userName),
    fPassword(password),
    fConnection(0),
    fSocket(-1),
    fCurrentChannel(RabbitMqAmqpImpl::INITIAL_CHANNEL)
{
}


RabbitMqAmqpImpl:: ~RabbitMqAmqpImpl()
{
    // nothing is done
}    


// RabbitMqAmqpImpl
// public member functions
RabbitMqAmqpImpl*
RabbitMqAmqpImpl::create(const string& hostName,
                         const int port,
                         const string& virtualHostName,
                         const string& userName,
                         const string& password)
{
    return ( new RabbitMqAmqpImpl(hostName,
                                 port,
                                 virtualHostName,
                                 userName,
                                 password) );
}

bool
RabbitMqAmqpImpl::isConnected()
{
    return ( (0 != fConnection) && (-1 != fSocket) );
}


int
RabbitMqAmqpImpl::connect()
{
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "connecting...\n");
    }
    
    int result = 1;
    
    if (!isConnected()) {
        // open a connection and socket
        fConnection = amqp_new_connection();
        fSocket = amqp_open_socket(fHostName.c_str(), fPort);
        
        if (-1 == fSocket) {
            return (0);
        }
        
        amqp_set_sockfd(fConnection, fSocket);

        // login
        amqp_rpc_reply_t login =
            amqp_login(fConnection,
                       fVirtualHostName.c_str(),
                       RabbitMqAmqpImpl::CHANNEL_MAX,
                       RabbitMqAmqpImpl::FRAME_MAX,
                       0, // heartbeat
                       AMQP_SASL_METHOD_PLAIN,
                       fUserName.c_str(),
                       fPassword.c_str());
        
        result = RabbitMqValidator::validate(login, "Logging in");
    }


    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "...connected\n");
    }
    
    return (result);
}


shared_ptr<Amqp::Channel>
RabbitMqAmqpImpl::open()
{
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "opening a channel...\n");
    }

    assert( fConnection != 0 );
    
    int result = 1;
    uint16_t channelValue = assignChannel();

    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "channel: %d\n", channelValue);
    }
    
    // open a channel    
    amqp_channel_open(fConnection, channelValue);
    result = RabbitMqValidator::validate(amqp_get_rpc_reply(fConnection), "Opening channel");

    shared_ptr<Amqp::Channel> channel;
    if (1 == result) {
        channel = shared_ptr<Amqp::Channel>( new RabbitMqChannelImpl(fConnection, channelValue) );
    }

    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "...opened a channel\n");
    }
    
    return ( channel );
}


int
RabbitMqAmqpImpl::disconnect()
{
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "disconnecting...\n");
    }

    // there is a case where the connection is already closed    
    if (!isConnected()) {
        return (1);
    }

    amqp_connection_close(fConnection, AMQP_REPLY_SUCCESS);    
    int result = RabbitMqValidator::validate(amqp_get_rpc_reply(fConnection), "Closing connection");

    if (0 < result) {
        amqp_destroy_connection(fConnection);
        RabbitMqValidator::validate(::close(fSocket), "Closing socket");
        fConnection = 0;
        fSocket = -1;
    }

    if (Debug::ON) {
        printf("...disconnected\n");
    }
    
    return result;
}


// RabbitMqAmqpImpl
// private functions
uint16_t
RabbitMqAmqpImpl::assignChannel()
{
    if (numeric_limits<uint16_t>::max() == fCurrentChannel) {
        fCurrentChannel = 0;
    }
    
    return (++fCurrentChannel);
}


// RabbitMqChannelImpl
// constructors and destructor
RabbitMqChannelImpl::RabbitMqChannelImpl(amqp_connection_state_t connection,
                                         uint16_t channel) :
    fConnection(connection),
    fChannel(channel)
{
}

RabbitMqChannelImpl::~RabbitMqChannelImpl()
{
    // nothing is done
}


// RabbitMqChannelImpl
// public functions
int
RabbitMqChannelImpl::setupQueue(const string& exchangeName,
                                const string& routingKey,
                                const string& queueName)
{
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "set up a queue...\n");
    }
    
    int result = 1;
    amqp_table_t emptyTable = {0, NULL};
    amqp_bytes_t exchangeNameByte = amqp_cstring_bytes(exchangeName.c_str());
    amqp_bytes_t queueNameByte = amqp_cstring_bytes(queueName.c_str());

    // declare an exchange
    amqp_exchange_declare(fConnection,
                          fChannel,
                          exchangeNameByte,
                          amqp_cstring_bytes("direct"), // exchange type : [direct|fanout|topic]
                          0, // passive
                          0, // durable
                          //0, // auto-delete
                          emptyTable);
    
    result = result & RabbitMqValidator::validate(amqp_get_rpc_reply(fConnection), "Declaring exchange");

    
    // declare a queue
   amqp_queue_declare(fConnection,
                      fChannel,
                      queueNameByte,
                      0, // passive
                      0, // durable
                      0, // exclusive
                      1, // auto_delete
                      emptyTable);

   result = result & RabbitMqValidator::validate(amqp_get_rpc_reply(fConnection), "Declaring queue");

    
    // bind the queue with the exchange and routing key
   amqp_queue_bind(fConnection,
                   fChannel,
                   queueNameByte,
                   exchangeNameByte,
                   amqp_cstring_bytes(routingKey.c_str()),
                   emptyTable);
    
   result = result & RabbitMqValidator::validate(amqp_get_rpc_reply(fConnection), "Binding queue");
    
   if (Debug::ON) {
       Debug::LOG(MODULE, __FILE__, __LINE__,
                  "exchange: %s, queue: %s, routing key: %s\n",
                  exchangeName.c_str(), queueName.c_str(), routingKey.c_str());
   }

   return result;
}


int
RabbitMqChannelImpl::subscribe(const string& queueName)
{
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "subscribing a queue...\n");
    }
    

    // subscribe
    amqp_bytes_t emptyBytes = {0, NULL};
    amqp_table_t emptyTable = {0, NULL};    
    amqp_bytes_t queueNameByte = amqp_cstring_bytes(queueName.c_str());    
    amqp_basic_consume(fConnection,
                       fChannel,
                       queueNameByte,
                       emptyBytes, // consumer tag
                       0, // no local
                       1, // no ack
                       0, // exclusive
                       emptyTable);
    
    int result = RabbitMqValidator::validate(amqp_get_rpc_reply(fConnection), "Consuming");

    
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "queue: %s\n", queueName.c_str());
        Debug::LOG(MODULE, __FILE__, __LINE__, "...subscribed a queue\n");
    }
    
    return result;
}

// when non-empty replyTo is specified, it should be subscribed before this method is called
// when an empty replyTo is specified, this method call is treated as a reply
int
RabbitMqChannelImpl::publish(const string& exchangeName,
                             const string& routingKey,
                             const message_string& message,
                             const string& replyTo)
{
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "publishing a message...\n");
    }

    // NOTE: 
    // If no properties need to be sent, a NULL MUST be specified to
    // the properties parameter of amqp_basic_publish.
    // Otherwise, the function will not work as intended
    // even if a correct exchange and a routing key are specified!!
    amqp_basic_properties_t *properties = NULL;
    
    amqp_basic_properties_t p;
    if (replyTo.empty() != true) {
        p._flags = AMQP_BASIC_REPLY_TO_FLAG;
        p.reply_to = amqp_cstring_bytes(replyTo.c_str());
        properties = &p;
    }

    uint8_t *body = const_cast<uint8_t *>(message.data());
    size_t length = message.length();
    amqp_bytes_t messageBody = {length, body};
    amqp_basic_publish(fConnection,
                       fChannel,
                       amqp_cstring_bytes(exchangeName.c_str()),
                       amqp_cstring_bytes(routingKey.c_str()),
                       0, // mandatory
                       0, // immediate
                       properties,
                       messageBody);
    
    int result = RabbitMqValidator::validate(amqp_get_rpc_reply(fConnection), "Publishing");

    
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "sending: \n");
        Debug::LOG(MODULE, __FILE__, __LINE__,
                   "exchange: %s, routing key: %s, reply to: %s\n",
                   exchangeName.c_str(), routingKey.c_str(), replyTo.c_str());
        Debug::DUMP_MESSAGE(MODULE, message.c_str(), message.length());
        Debug::LOG(MODULE, __FILE__, __LINE__, "...published a message\n");
    }

    return result;
}


void
RabbitMqChannelImpl::waitForMessage(message_string& message,
                                    string& replyTo)
{
    // read the frame method, basic deliver method and frame header
    amqp_frame_t frame;
    int result;

    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "waiting for a message...\n");
    }
    
    while(true) {
        // wait for a frame method
        amqp_maybe_release_buffers(fConnection);
        result = amqp_simple_wait_frame(fConnection, &frame);

        RabbitMqValidator::validate(result, "Waiting for frames");
            
        // if a frame method is received, then check the method id
        // wait for the delivery method
        if (frame.frame_type != AMQP_FRAME_METHOD) {
            continue;
        }

        if (frame.payload.method.id == AMQP_BASIC_DELIVER_METHOD) {
            break;
        }
    }

    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "received frame method and deliver method\n");
    }


    // the delivery method should be received at this point
    // then wait for the header
    result = amqp_simple_wait_frame(fConnection, &frame);
    RabbitMqValidator::validate(result, "Waiting for the header");

    if (frame.frame_type != AMQP_FRAME_HEADER) {
        RabbitMqValidator::validate(-1, "Unexpected header! : AMQP_FRAME_HEADER expected");
    }

    if(Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "received frame header\n");
    }

    if (true == replyTo.empty()) {
        amqp_basic_properties_t* properties =
            static_cast<amqp_basic_properties_t *>(frame.payload.properties.decoded);
        
        if (properties->_flags & AMQP_BASIC_REPLY_TO_FLAG) {
            replyTo = string(static_cast<const char *>(properties->reply_to.bytes),
                             static_cast<string::size_type>(properties->reply_to.len));
        }
        else {
            RabbitMqValidator::validate(-1, "Unexpected property!");
        }
    }

    
    if(Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__,
                   "channel: %d, reply to: %s\n",
                   frame.channel, replyTo.c_str());
    }
    

    message_string body;    
    size_t bodyTarget = frame.payload.properties.body_size;
    size_t bodyReceived = 0;

    while (bodyReceived < bodyTarget) {
        result = amqp_simple_wait_frame(fConnection, &frame);
        RabbitMqValidator::validate(result, "Waiting for the message body");        

        if (frame.frame_type != AMQP_FRAME_BODY) {
            RabbitMqValidator::validate(-1, "Unexpected header! : AMQP_FRAME_HEADER expected");
        }

        uint8_t* bytes = (uint8_t*)frame.payload.body_fragment.bytes;
        size_t len = frame.payload.body_fragment.len;

        body.append(bytes, len);
        bodyReceived += len;

        assert(bodyReceived <= bodyTarget);
    }
    assert (bodyReceived == bodyTarget);

    message = body;

    if(Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "receiving: \n");
        Debug::DUMP_MESSAGE(MODULE, message.c_str(), message.length());
    }

}

int
RabbitMqChannelImpl::close()
{
    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "closing a channel...\n");
    }
    
    amqp_channel_close(fConnection, fChannel, AMQP_REPLY_SUCCESS);
    int result = RabbitMqValidator::validate(amqp_get_rpc_reply(fConnection), "Closing channel");

    if (Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "...closed a channel\n");
    }
    
    return result;
}


// RabbitMqValidator
// public memeber functions
void
RabbitMqValidator::validate(int result, const string& context)
{
    if (result < 0) {
        fprintf(stderr, "%s: %s\n", context.c_str(), strerror(-result));        
        exit(1);
    }
}

int
RabbitMqValidator::validate(amqp_rpc_reply_t result, const string& context)
{
    switch (result.reply_type) {
    case AMQP_RESPONSE_NORMAL:
        return (1);

    case AMQP_RESPONSE_NONE:
        fprintf(stderr, "%s: missing RPC reply type!", context.c_str());        
        break;

    case AMQP_RESPONSE_LIBRARY_EXCEPTION: {
        const char *error = (result.library_error ? strerror(result.library_error) : "(end-of-stream)");
        fprintf(stderr, "%s: %s\n", context.c_str(), error);
        break;        
    }

    case AMQP_RESPONSE_SERVER_EXCEPTION:
        switch (result.reply.id) {
        case AMQP_CONNECTION_CLOSE_METHOD: {
            amqp_connection_close_t *m = (amqp_connection_close_t *) result.reply.decoded;
            fprintf(stderr, "%s: server connection error %d, message: %.*s",
                    context.c_str(),
                    m->reply_code,
                    (int) m->reply_text.len, (char *) m->reply_text.bytes);
            break;
        }
        case AMQP_CHANNEL_CLOSE_METHOD: {
            amqp_channel_close_t *m = (amqp_channel_close_t *) result.reply.decoded;
            fprintf(stderr, "%s: server channel error %d, message: %.*s",
                    context.c_str(),
                    m->reply_code,
                    (int) m->reply_text.len, (char *) m->reply_text.bytes);            
            break;
        }
        default:
            fprintf(stderr, "%s: unknown server error, method id 0x%08X", context.c_str(), result.reply.id);            
            break;
        }
        break;
    }

    return (0);
}
