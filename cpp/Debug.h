// File: Debug.h - last edit:
// Chiharu Kawatake	07-03-2009
// Copyright (c) 2009 by Chiharu Kawatake
// All rights reserved.

#ifndef _WASTE_DEBUG_H
#define _WASTE_DEBUG_H

#include <stdio.h>
#include <stdarg.h>
#include <syslog.h>

#include <string>

#include <boost/date_time/posix_time/posix_time.hpp>

#include "AmqpInterface.h"

namespace waste {

    class Debug {
    public:
        enum {ON = 1};

        static void LOG(const char* module, const char* fileName, int lineNumber, const char* format, ...) {
            va_list log;
            va_start(log, format);

            boost::posix_time::ptime now(boost::posix_time::microsec_clock::local_time());
            std::string time = to_iso_string(now);

            openlog(module, LOG_PERROR | LOG_PID, LOG_USER);
            syslog(LOG_DEBUG, "[%s,%d] [%s]", fileName, lineNumber, time.c_str());
            vsyslog(LOG_DEBUG, format, log);
            closelog();
            va_end(log);
        }

        static void DUMP_MESSAGE(const char* module, const unsigned char* message, size_t length) {

            const char* format = "0x00 ";
            char byte[strlen(format) + 1];
            std::string buffer;

            for (size_t i=0; i<length; i++) {
                sprintf(byte, "0x%02x ", message[i]);
                buffer.append(byte);
            }

            openlog(module, LOG_PERROR | LOG_PID, LOG_USER);
            syslog(LOG_DEBUG, "%s", buffer.c_str());
            closelog();
        }
    };

} 

#endif /* !_WASTE_DEBUG_H */

// LOG:
// 07-03-2009 Chiharu Kawatake	created

