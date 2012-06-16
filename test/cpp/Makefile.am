AM_CXXFLAGS = -Wall 
AM_CPPFLAGS = -I. -I../../lib/cpp/include -I./gen-cpp $(BOOST_CPPFLAGS) $(THRIFT_CPPFLAGS) $(RABBITMQ_C_CPPFLAGS)
AM_LDFLAGS = -L../../lib/cpp $(THRIFT_LDFLAGS) $(RABBITMQ_C_LDFLAGS) $(THRIFT_LIBS) $(RABBITMQ_C_LIBS) $(WASTE_LIBS) -lwaste

TESTCPPFLAGS = $(AM_CPPFLAGS) 

TESTLDFLAGS = $(AM_LDFLAGS) 

bin_PROGRAMS =	\
TestServer		\
TestClient

TestServer_SOURCES =       \
TestServer.cpp	           \
gen-cpp/Test.cpp           \
gen-cpp/Test_constants.cpp \
gen-cpp/Test_types.cpp     \
gen-cpp/Test_types.h

TestServer_CPPFLAGS = $(AM_CPPFLAGS)

TestServer_LDFLAGS = $(AM_LDFLAGS)

TestServer.cpp : gen-cpp/Test_types.h

TestClient_SOURCES =       \
TestClient.cpp         \
gen-cpp/Test.cpp           \
gen-cpp/Test_constants.cpp \
gen-cpp/Test_types.cpp     \
gen-cpp/Test_types.h

TestClient_CPPFLAGS = $(AM_CPPFLAGS)

TestClient_LDFLAGS = $(AM_LDFLAGS)

TestClient.cpp : gen-cpp/Test_types.h

THRIFT = thrift

gen-cpp/Test.cpp gen-cpp/Test_constants.cpp gen-cpp/Test_types.cpp gen-cpp/Test_types.h : ../Test.thrift
	$(THRIFT) --gen cpp:dense $<

clean-local:
	$(RM) -r gen-cpp

