#
ERLC		=	erlc
ERL			=	erl -boot start_clean
OBJDIR		=	../../lib/erl/ebin
INCDIR		=	gen-erl
THRIFTDIR	=	gen-erl
THRIFTFILE	=	../Test.thrift
THRIFT		=	thrift

SRCS		=	$(wildcard *.erl) $(wildcard $(THRIFTDIR)/*.erl)
INCLUDES	=	$(wildcard $(INCDIR)/*.hrl)
OBJS		=	$(SRCS:%.erl=$(OBJDIR)/%.beam)

EFLAGS		=	-I $(INCDIR) -o $(OBJDIR) -W +debug_info -smp -Wall -pa $(OBJDIR) -v

all: $(THRIFTDIR)/ $(OBJS)

$(THRIFTDIR)/: $(THRIFTFILE)
	$(THRIFT) --gen erl $(THRIFTFILE)

$(OBJDIR)/%.beam: %.erl $(INCLUDES)
	erlc $(EFLAGS) $<

clean:
	rm -rf $(OBJDIR) $(THRIFTDIR) erl_crash.dump
