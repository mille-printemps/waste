TARGET          = cpp test erl #tokenizer
LOGFILE         = compile.log
DATE_COMMAND    = /bin/date
TEE_COMMAND     = /usr/bin/tee -a
ECHO_COMMAND    = /bin/echo
RM              = /bin/rm -f

START_MSG       = "*** Rebuilding *** Start time: `$(DATE_COMMAND)`"
HOST_MSG        = "(`/usr/bin/uname -a`)"
END_MSG         = "End time: `$(DATE_COMMAND)`"
CLEAN_MSG       = "*** Cleaning Directories ***"
CLEANTP_MSG     = "*** Cleaning Templates Database ***"

all:
	@$(ECHO_COMMAND) $(START_MSG) 2>&1 | $(TEE_COMMAND) $(LOGFILE)
	@$(ECHO_COMMAND) $(HOST_MSG) 2>&1 | $(TEE_COMMAND) $(LOGFILE)
	@for i in $(TARGET); do \
	cd $$i; \
	$(ECHO_COMMAND) "Rebuilding $$i ..."; \
	$(ECHO_COMMAND) "(Subsystem compilation start: `$(DATE_COMMAND)`)"; \
	make; \
	$(ECHO_COMMAND) "(Subsystem compilation end:   `$(DATE_COMMAND)`)"; \
	$(ECHO_COMMAND) Done.; \
	cd ..; \
	done 2>&1 | $(TEE_COMMAND) -a $(LOGFILE)
	@$(ECHO_COMMAND) $(END_MSG) 2>&1 | $(TEE_COMMAND) $(LOGFILE)

clean: 
	@$(RM) $(LOGFILE)
	@$(ECHO_COMMAND) $(CLEAN_MSG) 2>&1 | $(TEE_COMMAND) $(LOGFILE)
	@for i in $(TARGET); do \
	cd $$i; \
	$(ECHO_COMMAND);$(ECHO_COMMAND)  clean $$i; \
	make clean; \
	$(ECHO_COMMAND) Done.; \
	cd ..; \
	done 2>&1 | $(TEE_COMMAND) $(LOGFILE); \
