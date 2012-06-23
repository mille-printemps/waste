ERLC		=	erlc
ERL			=	erl -boot start_clean
SRCDIR		=	src
OBJDIR		=	ebin
INCDIR		=	$(DEPSDIR)/rabbit_common/include $(DEPSDIR)/amqp_client/include
DEPSDIR		=	deps
ERLLIBS		=	$(DEPSDIR)

SRCS		=	$(wildcard $(SRCDIR)/*.erl)
INCLUDES	=	$(wildcard $(foreach i,$(INCDIR), $i/*.hrl))
OBJS		=	$(SRCS:%.erl=$(OBJDIR)/%.beam)

EFLAGS		=	$(foreach i,$(INCDIR), -I $i) -o $(OBJDIR) +debug_info -smp -Wall -pa $(OBJDIR) -pa $(THRIFT_LIBPATH)/erlang/lib/thrift-0.4.0/ebin -v

all: $(OBJDIR) $(OBJS)

$(OBJDIR):
	mkdir $(OBJDIR) $(DEPSDIR)
	unzip -d $(DEPSDIR) ../../deps/rabbitmq-erlang-client/dist/amqp_client-0.0.0.ez
	unzip -d $(DEPSDIR) ../../deps/rabbitmq-erlang-client/dist/rabbit_common-0.0.0.ez
	ln -s rabbit_common-0.0.0 $(DEPSDIR)/rabbit_common
	ln -s amqp_client-0.0.0 $(DEPSDIR)/amqp_client

$(OBJS): $(OBJDIR)/%.beam: %.erl $(INCLUDES)
	ERL_LIBS=$(ERLLIBS) erlc $(EFLAGS) $<

clean:
	rm -rf $(OBJDIR) $(DEPSDIR)
