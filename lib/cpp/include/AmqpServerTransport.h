// File: AmqpServerTransport.h - last edit:
// Chiharu Kawatake	05-30-2012
// Copyright (c) 2012 by Chiharu Kawatake
// All rights reserved.

#ifndef _WASTE_AMQP_SERVER_TRANSPORT_H
#define _WASTE_AMQP_SERVER_TRANSPORT_H

#include <string>
#include <boost/shared_ptr.hpp>
#include <thrift/transport/TTransport.h>
#include <thrift/transport/TServerTransport.h>

#include "AmqpInterface.h"

namespace waste {

    class AmqpServerTransport : public apache::thrift::transport::TServerTransport {

    public:
        // constructors and destructor
        AmqpServerTransport(boost::shared_ptr<Amqp::Channel> channel,
                            const std::string& exchangeName,
                            const std::string& routingKey,
                            const std::string& queueName);

        ~AmqpServerTransport();
        
        // public functions
        void close();

    protected:
        // overriding protected functions
        boost::shared_ptr<apache::thrift::transport::TTransport> acceptImpl();

    private:
        // private static variables
        static const char* MODULE;

        // private variables
        boost::shared_ptr<Amqp::Channel> fChannel;
        std::string fExchangeName;
        std::string fRoutingKey;
        std::string fQueueName;
    };

} 

#endif /* !_WASTE_AMQP_SERVER_TRANSPORT_H */

// LOG:
// 06-21-2009 Chiharu Kawatake	created
