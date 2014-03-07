#ifndef _WASTE_AMQP_TRANSPORT_H
#define _WASTE_AMQP_TRANSPORT_H

#include <cstdint>
#include <string>
#include <boost/shared_ptr.hpp>
#include <thrift/transport/TTransportException.h>
#include <thrift/transport/TBufferTransports.h>

#include "AmqpInterface.h"

namespace waste {

    class AmqpTransport : public apache::thrift::transport::TMemoryBuffer {
        
    public:
        // constructors and destructor
        // this constructor is for clients
        AmqpTransport(boost::shared_ptr<Amqp::Channel> channel,
                      const std::string& exchangeName,
                      const std::string& routingKey,
                      const std::string& queueName);

        ~AmqpTransport();

        // public functions
        bool isOpen();
        void open();
        std::uint32_t read(std::uint8_t* buf, std::uint32_t len);
        void write(const std::uint8_t* buf, std::uint32_t len);
        void close();
        void flush();

    private:
        // private constants
        static const char* MODULE;
        static const char* NO_REPLY_TO;

        // private variables
        boost::shared_ptr<Amqp::Channel> fChannel;
        std::string fExchangeName;
        std::string fRoutingKey;
        std::string fReplyTo;
        std::string fQueueName;
        bool fIsWaitingForMessage;

        // private functions
        std::string uuid();
        void resetBuffer();
        bool isClient();
    };

} 

#endif /* !_WASTE_AMQP_TRANSPORT_H */
