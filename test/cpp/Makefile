CXX			=	g++
CFLAGS		=	-O0 -Wall -g #-Werror
INCDIR		=	-I. -I../gen-cpp -I../../include \
				-I/usr/local/include/thrift \
				-I/usr/local/include -I/usr/include \
				-I/opt/local/include
LIBSDIR		=	-L../../lib \
				-L/usr/local/lib 
LIBS		=	-lthrift -lamqp -lrabbitmq

CPPFLAGS	=	$(CFLAGS) $(INCDIR) 
LDFLAGS		=	$(LIBSDIR)

OBJDIR		=	
LIBDIR		=    
EXEDIR		=	
THRIFTDIR	=	../gen-cpp
SRCS		=	TestServer.cpp TestClient.cpp $(THRIFTDIR)/Test_constants.cpp $(THRIFTDIR)/Test_types.cpp $(THRIFTDIR)/Test.cpp

OBJ			=	${SRCS:%.cpp=%.o}
EXE			=	


default: server client

server: $(OBJ)
	$(CXX) $(LDFLAGS) -o TestServer ${filter-out TestClient.o, $(OBJ)} $(LIBS)

client: $(OBJ)
	$(CXX) $(LDFLAGS) -o TestClient ${filter-out TestServer.o, $(OBJ)} $(LIBS)

$(OBJ): %.o: %.cpp
	$(CXX) $(CPPFLAGS) -c ${patsubst %.o, %.cpp, $@} -o $@

clean:
	rm -f $(OBJ) TestServer TestClient
