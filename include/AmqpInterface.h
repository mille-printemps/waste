// File: Amqp.h - last edit:
// Chiharu Kawatake	06-25-2009
// Copyright (c) 2009 by Chiharu Kawatake
// All rights reserved.

#ifndef _WASTE_AMQP_INTERFACE_H
#define _WASTE_AMQP_INTERFACE_H

#include <string>
#include <boost/shared_ptr.hpp>

namespace waste {

    typedef std::basic_string<uint8_t> message_string;
    
    class Amqp {
    public:
        
        class Channel {
        public:
            virtual ~Channel();

            /**
               declares the exchange and the queue and bind them with the routing key
            */
            virtual int setupQueue(const std::string& exchangeName,
                                   const std::string& routingKey,
                                   const std::string& queueName) = 0;

            /**
               subscribes the queue
            */
            virtual int subscribe(const std::string& queuename) = 0;

            /**
               publish a message
            */
            virtual int publish(const std::string& exchangeName,
                                const std::string& routingKey,
                                const message_string& message,
                                const std::string& replyTo) = 0;
            /**
               waits for a message
            */
            virtual void waitForMessage(message_string& message,
                                        std::string& replyTo) = 0;

            /**
               close the channel
            */

            virtual int close() = 0;
        };

        virtual ~Amqp();

        /**
           indicates whether a client or server is connected to the broker
        */
        virtual bool isConnected() = 0;

        /**
           connects to the broker
        */
        virtual int connect() = 0;

        /**
           opens a channel
           the client is responsible for releasing the memory of Channel object
        */
        virtual boost::shared_ptr<Channel> open() = 0;

        /**
           disconnects from the broker
        */
        virtual int disconnect() = 0;
    };
    
}

#endif /* !_WASTE_AMQP_INTERFACE_H */

// LOG:
// 06-25-2009 Chiharu Kawatake	created

