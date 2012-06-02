CXX		=	g++
CFLAGS	=	-O2 -Wall -g #-DDEBUG #-Werror
INCDIR	=	-I.\
			-I../include\
	        -I/usr/local/include/thrift\
			-I/opt/local/include\
			-I/usr/include
LIBSDIR	=	
LIBS	=	

CPPFLAGS	=	$(CFLAGS) $(INCDIR) 
LDFLAGS		=	$(LIBSDIR)

OBJDIR	=	../bin
LIBDIR	=   ../lib
EXEDIR	=	
SRCS	=	$(wildcard *.cpp)
OBJ		=	${SRCS:%.cpp=$(OBJDIR)/%.o}
LIB     =   $(LIBDIR)/libamqp.a
EXE		=	


$(LIB): $(OBJ)
	ar cr $(LIB) $(OBJ)
	ranlib $(LIB)

$(OBJ): $(OBJDIR)/%.o: %.cpp
	$(CXX) $(CPPFLAGS) -c ${patsubst $(OBJDIR)/%.o, %.cpp, $@} -o $@

clean:
	rm -f $(OBJ) $(LIB)
