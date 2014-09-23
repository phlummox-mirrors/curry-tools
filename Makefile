# Generate various tools for Curry

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

export BINDIR     = $(ROOT)/bin
export LIBDIR     = $(ROOT)/lib
export METADIR    = $(LIBDIR)/meta
export CLEANCURRY = $(BINDIR)/cleancurry

.PHONY: all
all:
	@cd optimize       && $(MAKE)
	@cd addtypes       && $(MAKE)
	@cd browser        && $(MAKE)
	@cd CASS           && $(MAKE)
	@cd currydoc       && $(MAKE)
	@cd curry2js       && $(MAKE)
	@cd currypp        && $(MAKE)
	@cd createmakefile && $(MAKE)
	@cd currytest      && $(MAKE)
	@cd erd2curry      && $(MAKE)
	@cd genint         && $(MAKE)
	@cd importcalls    && $(MAKE)
	@cd spicey         && $(MAKE)
	@cd typeinference  && $(MAKE)
	@cd xmldata        && $(MAKE)

.PHONY: currydoc
currydoc:
	cd currydoc && $(MAKE)

.PHONY: typeinference
typeinference:
	cd typeinference && $(MAKE)

.PHONY: clean
clean:
	cd optimize       && $(MAKE) clean
	cd addtypes       && $(MAKE) clean
	cd analysis       && $(CLEANCURRY)
	cd browser        && $(MAKE) clean
	cd CASS           && $(MAKE) clean
	cd currydoc       && $(MAKE) clean
	cd curry2js       && $(MAKE) clean
	cd currypp        && $(MAKE) clean
	cd createmakefile && $(MAKE) clean
	cd currytest      && $(MAKE) clean
	cd erd2curry      && $(MAKE) clean
	cd genint         && $(MAKE) clean
	cd importcalls    && $(MAKE) clean
	cd spicey         && $(MAKE) clean
	cd typeinference  && $(MAKE) clean
	cd xmldata        && $(MAKE) clean
