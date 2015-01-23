# Generate various tools for Curry

# Required:
# - root location of the Curry System specified by variable ROOT

ROOT = $(error Please specify the variable ROOT)

export BINDIR     = $(ROOT)/bin
export LIBDIR     = $(ROOT)/lib
export METADIR    = $(LIBDIR)/meta
export CLEANCURRY = $(BINDIR)/cleancurry
export REPL       = $(BINDIR)/curry

.PHONY: all
all:
	@cd addtypes       && $(MAKE)
	@cd analysis       && $(MAKE)
	@cd browser        && $(MAKE)
	@cd CASS           && $(MAKE)
	@cd createmakefile && $(MAKE)
	@cd curry2js       && $(MAKE)
	@cd currydoc       && $(MAKE)
	@cd currypp        && $(MAKE)
	@cd currytest      && $(MAKE)
	@cd erd2curry      && $(MAKE)
	@cd genint         && $(MAKE)
	@cd importcalls    && $(MAKE)
	@cd optimize       && $(MAKE)
	@cd spicey         && $(MAKE)
	@cd typeinference  && $(MAKE)
	@cd xmldata        && $(MAKE)

.PHONY: currydoc
currydoc:
	cd currydoc && $(MAKE)

.PHONY: typeinference
typeinference:
	cd typeinference && $(MAKE)

.PHONY: compile
compile:
	@cd addtypes       && $(MAKE) compile
	@cd analysis       && $(MAKE) compile
	@cd browser        && $(MAKE) compile
	@cd CASS           && $(MAKE) compile
	@cd createmakefile && $(MAKE) compile
	@cd curry2js       && $(MAKE) compile
	@cd currydoc       && $(MAKE) compile
	@cd currypp        && $(MAKE) compile
	@cd currytest      && $(MAKE) compile
	@cd erd2curry      && $(MAKE) compile
	@cd genint         && $(MAKE) compile
	@cd importcalls    && $(MAKE) compile
	@cd optimize       && $(MAKE) compile
	@cd spicey         && $(MAKE) compile
	@cd typeinference  && $(MAKE) compile
	@cd xmldata        && $(MAKE) compile

.PHONY: install
install:
	@cd addtypes       && $(MAKE) install
	@cd analysis       && $(MAKE) install
	@cd browser        && $(MAKE) install
	@cd CASS           && $(MAKE) install
	@cd createmakefile && $(MAKE) install
	@cd curry2js       && $(MAKE) install
	@cd currydoc       && $(MAKE) install
	@cd currypp        && $(MAKE) install
	@cd currytest      && $(MAKE) install
	@cd erd2curry      && $(MAKE) install
	@cd genint         && $(MAKE) install
	@cd importcalls    && $(MAKE) install
	@cd optimize       && $(MAKE) install
	@cd spicey         && $(MAKE) install
	@cd typeinference  && $(MAKE) install
	@cd xmldata        && $(MAKE) install

.PHONY: clean
clean:
	@cd addtypes       && $(MAKE) clean
	@cd analysis       && $(MAKE) clean
	@cd browser        && $(MAKE) clean
	@cd CASS           && $(MAKE) clean
	@cd createmakefile && $(MAKE) clean
	@cd curry2js       && $(MAKE) clean
	@cd currydoc       && $(MAKE) clean
	@cd currypp        && $(MAKE) clean
	@cd currytest      && $(MAKE) clean
	@cd erd2curry      && $(MAKE) clean
	@cd genint         && $(MAKE) clean
	@cd importcalls    && $(MAKE) clean
	@cd optimize       && $(MAKE) clean
	@cd spicey         && $(MAKE) clean
	@cd typeinference  && $(MAKE) clean
	@cd xmldata        && $(MAKE) clean
