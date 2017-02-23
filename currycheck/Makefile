# Makefile for generating the currycheck tool

TOOL=$(BINDIR)/$(CURRYSYSTEM)-check

# currycheck (main) dependencies:
DEPS = *.curry $(LIBDIR)/Test/EasyCheck.curry $(LIBDIR)/AbstractCurry/*.curry

.PHONY: all compile install clean uninstall runtest

all: install

compile: CurryCheck

install: CurryCheck
	rm -f $(TOOL) $(BINDIR)/currycheck
	cd $(BINDIR) && ln -s ../currytools/currycheck/CurryCheck $(notdir $(TOOL))

clean:
	$(CLEANCURRY)
	rm -f CurryCheck

uninstall: clean
	rm -f $(TOOL)

CurryCheck: $(DEPS)
	$(REPL) $(REPL_OPTS) :l CurryCheck :save :q

# run the test suite
runtest:
	cd Examples                  && ./test.sh $(RUNTESTPARAMS)
	cd Examples/WithVerification && ./test.sh $(RUNTESTPARAMS)
