#
ERLC		=	erlc
ERL			=	erl -boot start_clean
SRCDIR		=	src
OBJDIR		=	ebin
INCDIR		=	$(SRCDIR)/lib/rabbit_common/include $(SRCDIR)/lib/amqp_client/include
ERLLIBS		=	$(SRCDIR):$(SRCDIR)/lib

SRCS		=	$(wildcard $(SRCDIR)/*.erl)
INCLUDES	=	$(wildcard $(foreach i,$(INCDIR), $i/*.hrl))
OBJS		=	$(SRCS:$(SRCDIR)/%.erl=$(OBJDIR)/%.beam)

EFLAGS		=	$(foreach i,$(INCDIR), -I $i) -o $(OBJDIR) -W +debug_info -smp -pa $(OBJDIR) -Wall -v

all: $(OBJDIR)/ $(OBJS)

$(OBJDIR)/:
	mkdir $(OBJDIR)
	mkdir -p $(SRCDIR)/lib
	unzip -d $(SRCDIR)/lib ../../deps/rabbitmq-erlang-client/dist/amqp_client-0.0.0.ez
	unzip -d $(SRCDIR)/lib ../../deps/rabbitmq-erlang-client/dist/rabbit_common-0.0.0.ez
	ln -s rabbit_common-0.0.0 src/lib/rabbit_common
	ln -s amqp_client-0.0.0 src/lib/amqp_client

$(OBJDIR)/%.beam: src/%.erl $(INCLUDES)
	ERL_LIBS=$(ERLLIBS) erlc $(EFLAGS) $<

clean:
	rm -rf $(OBJDIR) $(SRCDIR)/lib erl_crash.dump
