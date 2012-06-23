#ifndef _WASTE_AMQP_IMPL_H
#define _WASTE_AMQP_IMPL_H

#include <string>
#include <boost/shared_ptr.hpp>

#include "amqp.h"

#include "AmqpFactory.h"

namespace waste {

    class RabbitMqAmqpImpl : public AmqpFactory {
    private:
        // constructors
        RabbitMqAmqpImpl(const std::string& hostName,
                         const int port,
                         const std::string& virtualHostName,
                         const std::string& userName,
                         const std::string& password);

    public:
        static const int FRAME_MAX;
        static const int CHANNEL_MAX;
        static const int DEFAULT_CHANNEL;
        static const std::string DEFAULT_VIRTUAL_HOST_NAME;

        static RabbitMqAmqpImpl* create(const std::string& hostName,
                                        const int port,
                                        const std::string& virtualHostName,
                                        const std::string& userName,
                                        const std::string& password);

        // destructor
        ~RabbitMqAmqpImpl();

        // public functions
        bool isConnected();
        int connect();
        boost::shared_ptr<Amqp::Channel> open();
        int disconnect();

    private:
        // private static constants
        static const char* MODULE;
        static const uint16_t INITIAL_CHANNEL;

        // private variables
        std::string fHostName;
        int fPort;
        std::string fVirtualHostName;
        std::string fUserName;
        std::string fPassword;
        amqp_connection_state_t fConnection;
        int fSocket;
        uint16_t fCurrentChannel;

        // private functions
        uint16_t assignChannel();

        // avoids copying any instances of this class
        RabbitMqAmqpImpl(const RabbitMqAmqpImpl&);
        RabbitMqAmqpImpl& operator=(const RabbitMqAmqpImpl&);
    };

    class RabbitMqChannelImpl : public Amqp::Channel {
    public:
        // constructors and destructor
        RabbitMqChannelImpl(amqp_connection_state_t connection, uint16_t channel);
        ~RabbitMqChannelImpl();

        // public fuctions
        int setupQueue(const std::string& exchangeName,
                       const std::string& routingKey,
                       const std::string& queueName);

        int subscribe(const std::string& queuename);

        int publish(const std::string& exchangeName,
                    const std::string& routingKey,
                    const message_string& message,
                    const std::string& replyTo);

        void waitForMessage(message_string& message,
                            std::string& replyTo);

        int close();

    private:
        // private static constants
        static const char* MODULE;

        // private variables
        amqp_connection_state_t fConnection;
        uint16_t fChannel;
    };

    class RabbitMqValidator {
    public:
        static void validate(int result, const std::string& context);
        static int validate(amqp_rpc_reply_t result, const std::string& context);
    };

}

#endif /* !_WASTE_AMQP_IMPL_H */
