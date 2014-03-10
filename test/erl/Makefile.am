ERLC		=	erlc
ERL			=	erl -boot start_clean
OBJDIR		=	../../lib/erl/ebin
THRIFTDIR	=	gen-erl
INCDIR		=	$(THRIFTDIR)
THRIFTFILE	=	../Test.thrift
THRIFT		=	thrift

SRCS		=	$(wildcard *.erl) $(wildcard $(THRIFTDIR)/*.erl)
INCS		=	$(wildcard $(foreach i,$(INCDIR), $i/*.hrl))
OBJS		=	$(SRCS:%.erl=$(OBJDIR)/%.beam)

EFLAGS		=	$(foreach i,$(INCDIR), -I $i) -o $(OBJDIR) +debug_info -smp -Wall -pa $(OBJDIR) -pa $(ERLANG_INSTALL_LIB_DIR)/erlang/lib/thrift-0.9.0/ebin -v

ECHO		=	echo
TEE			=	tee -a
SCRIPT		=	client.sh

all: $(THRIFTDIR) $(OBJS) $(SCRIPT)

$(THRIFTDIR): $(THRIFTFILE)
	$(THRIFT) --gen erl $(THRIFTFILE)
	erlc $(EFLAGS) $(THRIFTDIR)/*.erl

$(OBJS): $(OBJDIR)/%.beam: %.erl $(THRIFTDIR)
	erlc $(EFLAGS) $<

$(SCRIPT): $(THRIFTDIR)
	touch $(SCRIPT)
	chmod 755 $(SCRIPT)
	@$(ECHO) '#!/bin/sh' 2>&1 | $(TEE) $(SCRIPT)
	@$(ECHO) '' 2>&1 | $(TEE) $(SCRIPT)
	@$(ECHO) 'ERL_THRIFT='$(ERLANG_INSTALL_LIB_DIR)'/erlang/lib/thrift-0.9.0' 2>&1 | $(TEE) $(SCRIPT)
	@$(ECHO) 'AMQP_SERVER=../../deps/rabbitmq-server' 2>&1 | $(TEE) $(SCRIPT)
	@$(ECHO) 'AMQP_CLIENT=../../deps/rabbitmq-erlang-client' 2>&1 | $(TEE) $(SCRIPT)
	@$(ECHO) 'POOLBOY=../../deps/poolboy' 2>&1 | $(TEE) $(SCRIPT)
	@$(ECHO) '' 2>&1 | $(TEE) $(SCRIPT)
	@$(ECHO) 'erl +K true -pa $${ERL_THRIFT}/ebin $${AMQP_CLIENT}/ebin $${AMQP_SERVER}/ebin $${POOLBOY}/ebin ../../lib/erl/ebin ./ -noshell -s test_client test -s init stop' 2>&1 | $(TEE) $(SCRIPT)

clean:
	rm -rf $(THRIFTDIR) $(SCRIPT) erl_crash.dump
