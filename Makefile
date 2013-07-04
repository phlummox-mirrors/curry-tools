# Generate various tools for Curry

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

.PHONY: all
all:
	@cd addtypes       && $(MAKE)
	@cd browser        && $(MAKE)
	@cd CASS           && $(MAKE)
	@cd currydoc       && $(MAKE)
	@cd curry2js       && $(MAKE)
	@cd createmakefile && $(MAKE)
	@cd currytest      && $(MAKE)
	@cd genint         && $(MAKE)
	@cd importcalls    && $(MAKE)
	@cd typeinference  && $(MAKE)

.PHONY: typeinference
typeinference:
	cd typeinference && $(MAKE)

.PHONY: clean
clean:
	cd addtypes       && $(MAKE) clean
	cd analysis       && $(ROOT)/bin/cleancurry
	cd browser        && $(MAKE) clean
	cd CASS           && $(MAKE) clean
	cd currydoc       && $(MAKE) clean
	cd curry2js       && $(MAKE) clean
	cd createmakefile && $(MAKE) clean
	cd currytest      && $(MAKE) clean
	cd genint         && $(MAKE) clean
	cd importcalls    && $(MAKE) clean
	cd typeinference  && $(MAKE) clean
