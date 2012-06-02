// File: AmqpTransport.cpp - last edit:
// Chiharu Kawatake	05-30-2012
// Copyright (c) 2012 by Chiharu Kawatake
// All rights reserved.

#include <assert.h>
#include <stdio.h>

#include <string>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "AmqpInterface.h"
#include "AmqpTransport.h"
#include "Debug.h"

using namespace waste;
using namespace boost;
using namespace boost::posix_time;
using namespace std;

const char* AmqpTransport::MODULE = "AmqpTransport";
const char* AmqpTransport::NO_REPLY_TO = "no-reply-to";

// constructors and destructor
AmqpTransport::AmqpTransport(shared_ptr<Amqp::Channel> channel,
                             const string& exchangeName,
                             const string& routingKey,
                             const string& queueName):
    TMemoryBuffer(),
    fChannel(channel),
    fExchangeName(exchangeName),
    fRoutingKey(routingKey),
    fQueueName(queueName),
    fIsWaitingForMessage(true)
{
    assert(fChannel.get() != NULL);
    
    // servers declare queues, so queueName is needed
    if ( false == isClient() ) {
        assert (queueName != "");
    }

    // clients do not declare queues for subscription
    if ( true == isClient() ) {
        assert (queueName == "");

        fReplyTo = uuid();

        if (Debug::ON) {
            Debug::LOG(MODULE, __FILE__, __LINE__, "reply to: %s\n", fReplyTo.c_str());
        }

        // name the routing key as the reply queue name
        // so that servers can publish their responses
        
        int result = fChannel->setupQueue(fExchangeName, fReplyTo, fReplyTo);
        result = result & fChannel->subscribe(fReplyTo);

        // TODO : does something for exceptional cases        
        
    }

}

AmqpTransport::~AmqpTransport()
{
    // nothing is done
}


// public member functions
bool
AmqpTransport::isOpen()
{
    return (true);
}

void
AmqpTransport::open()
{
    if(Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "opening...\n");
    }
    // nothing is done
}

uint32_t
AmqpTransport::read(uint8_t* buf, uint32_t len)
{
    if(Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "reading...\n");
    }

    if (true == fIsWaitingForMessage) {
        message_string message;
        string replyTo = NO_REPLY_TO; // just for making replyTo a non-empty string

        fChannel->waitForMessage(message, replyTo);
        this->write(message.data(), message.length());
    }
    
    return ( TMemoryBuffer::read(buf, len) );
}

void
AmqpTransport::write(const uint8_t* buf, uint32_t len)
{
    if(Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "writing...\n");
    }
    
    TMemoryBuffer::write(buf, len);
    fIsWaitingForMessage = false;
}

void
AmqpTransport::close()
{
    if(Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "closing...\n");
    }
    // does nothing
}

void
AmqpTransport::flush()
{
    if(Debug::ON) {
        Debug::LOG(MODULE, __FILE__, __LINE__, "flushing...\n");
    }

    // retrieve a message from the buffer
    uint8_t *body;
    uint32_t length;
    getBuffer(&body, &length);

    if (0 == length) {
        return;
    }
    
    message_string message = message_string(body, length);

    // then specify exchange, routing key and reply-to property
    // then publish
    
    string routingKey;
    string replyTo;

    if ( true == isClient() ) {
        routingKey = fRoutingKey;
        replyTo = fReplyTo;
    }
    else {
        routingKey = fQueueName;       
    }
    
    fChannel->publish(fExchangeName, routingKey, message, replyTo);

    // reset the values of the pointers to the buffer
    // once this method is called, the buffer waits for a message to come from a server
    
    resetBuffer();
    fIsWaitingForMessage = true;
}

// private member functions
string
AmqpTransport::uuid()
{
    ptime now(microsec_clock::local_time());
    return ( to_iso_string(now) );
}

void
AmqpTransport::resetBuffer()
{
    TMemoryBuffer newBuffer;
    this->swap(newBuffer);
}

bool
AmqpTransport::isClient()
{
    return ( "" != fRoutingKey );    
}


// LOG:
// 06-25-2009 Chiharu Kawatake	created

