#
ERLC		=	erlc
ERL			=	erl -boot start_clean
OBJDIR		=	../../ebin
INCDIR		=	../gen-erl
THRIFTDIR	=	../gen-erl

SRCS		=	$(wildcard *.erl) $(wildcard $(THRIFTDIR)/*.erl)
INCLUDES	=	$(wildcard $(INCDIR)/*.hrl)
OBJS		=	$(SRCS:%.erl=$(OBJDIR)/%.beam)

EFLAGS		=	-I $(INCDIR) -o $(OBJDIR) -W +debug_info -smp # -Wall +native -v

all: $(OBJS)

$(OBJDIR)/%.beam: %.erl $(INCLUDES)
	erlc $(EFLAGS) $<

clean:
	rm -rf $(OBJDIR)/*.beam erl_crash.dump
