#ifndef _WASTE_AMQP_FACTORY_H
#define _WASTE_AMQP_FACTORY_H

#include <cstdint>
#include <string>
#include <boost/shared_ptr.hpp>

#include "AmqpInterface.h"

namespace waste {

    class AmqpFactory : public Amqp {
    public:
        static boost::shared_ptr<Amqp> get(const std::string& hostName,
                                           const std::uint16_t port,
                                           const std::string& virtualHostName,
                                           const std::string& userName,
                                           const std::string& password);

        static Amqp* create(const std::string& hostName,
                            const std::uint16_t port,
                            const std::string& virtualHostName,
                            const std::string& userName,
                            const std::string& password);
    };

} 

#endif /* !_WASTE_AMQP_FACTORY_H */

