AUTOMAKE_OPTIONS = subdir-objects
AM_CXXFLAGS = -Wall 
AM_CPPFLAGS = -I./include $(BOOST_CPPFLAGS) $(THRIFT_CPPFLAGS) $(RABBITMQ_C_CPPFLAGS)

lib_LIBRARIES = libwaste.a

libwaste_a_SOURCES =        \
src/Amqp.cpp		        \
src/AmqpServerTransport.cpp \
src/RabbitMqAmqpImpl.cpp    \
src/AmqpFactory.cpp	        \
src/AmqpTransport.cpp       \
src/RabbitMqAmqpImpl.h      \
src/Debug.h

libwaste_a_CPPFLAGS = $(AM_CPPFLAGS)
