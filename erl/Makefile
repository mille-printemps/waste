#
ERLC		=	erlc
ERL			=	erl -boot start_clean
OBJ_DIR		=	../ebin
INCLUDE_DIR	=	lib/rabbit_common/include lib/amqp_client/include
ERL_LIBS	=	lib

SRCS		=	$(wildcard *.erl)
INCLUDES	=	$(wildcard $(foreach i,$(INCLUDE_DIR), $i/*.hrl))
OBJS		=	$(SRCS:%.erl=$(OBJ_DIR)/%.beam)

EFLAGS		=	$(foreach i,$(INCLUDE_DIR), -I $i) -o $(OBJ_DIR) -W +debug_info -smp # -Wall +native -v

all: $(OBJS)

$(OBJ_DIR)/%.beam: %.erl $(INCLUDES)
	ERL_LIBS=$(ERL_LIBS) erlc $(EFLAGS) $<

clean:
	rm -rf $(OBJ_DIR)/*.beam $(OBJ_DIR)/erl_crash.dump erl_crash.dump
