# Generate various tools for Curry

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

.PHONY: all
all:
	@cd browser     && $(MAKE)
	@cd currydoc    && $(MAKE)
	@cd currytest   && $(MAKE)
	@cd genint      && $(MAKE)
	@cd importcalls && $(MAKE)

.PHONY: clean
clean: 
	cd analysis    && $(ROOT)/bin/cleancurry
	cd browser     && $(MAKE) clean
	cd currydoc    && $(MAKE) clean
	cd currytest   && $(MAKE) clean
	cd genint      && $(MAKE) clean
	cd importcalls && $(MAKE) clean
