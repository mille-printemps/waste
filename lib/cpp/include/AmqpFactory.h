// File: AmqpFactory.h - last edit:
// Chiharu Kawatake	05-30-2012
// Copyright (c) 2012 by Chiharu Kawatake
// All rights reserved.

#ifndef _WASTE_AMQP_FACTORY_H
#define _WASTE_AMQP_FACTORY_H

#include <string>
#include <boost/shared_ptr.hpp>

#include "AmqpInterface.h"

namespace waste {

    class AmqpFactory : public Amqp {
    public:
        static boost::shared_ptr<Amqp> get(const std::string& hostName,
                                           const int port,
                                           const std::string& virtualHostName,
                                           const std::string& userName,
                                           const std::string& password);

        static Amqp* create(const std::string& hostName,
                            const int port,
                            const std::string& virtualHostName,
                            const std::string& userName,
                            const std::string& password);
    };

} 

#endif /* !_WASTE_AMQP_FACTORY_H */

// LOG:
// 06-25-2009 Chiharu Kawatake	created

