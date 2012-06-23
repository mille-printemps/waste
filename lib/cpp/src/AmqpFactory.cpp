#include <string>
#include <boost/shared_ptr.hpp>

#include "RabbitMqAmqpImpl.h"
#include "AmqpFactory.h"

using namespace waste;
using namespace boost;
using namespace std;

// public member functions
shared_ptr<Amqp>
AmqpFactory::get(const string& hostName,
                 const int port,
                 const string& virtualHostName,
                 const string& userName,
                 const string& password)
{
    return ( shared_ptr<Amqp>( RabbitMqAmqpImpl::create(hostName,
                                                        port,
                                                        virtualHostName,
                                                        userName,
                                                        password)) );
}

Amqp*
AmqpFactory::create(const string& hostName,
                    const int port,
                    const string& virtualHostName,
                    const string& userName,
                    const string& password)
{
    return ( RabbitMqAmqpImpl::create(hostName,
                                      port,
                                      virtualHostName,
                                      userName,
                                      password) );
}
