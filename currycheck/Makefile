# Makefile for generating the currycheck tool

TOOL=$(BINDIR)/$(CURRYSYSTEM)-check

# replacement stuff
empty     :=
space     := $(empty) $(empty)

# The load path of the browser:
LOADPATH = src:$(subst $(space),:,$(wildcard .cpm/packages/*/src))

# Source modules:
DEPS = .cpm/packages/*/src/*/*curry src/*.curry \
       $(LIBDIR)/Test/EasyCheck.curry $(LIBDIR)/AbstractCurry/*.curry

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
	$(REPL) $(REPL_OPTS) :set path $(LOADPATH) :l CurryCheck :save :q

# run the test suite
runtest:
	$(BINDIR)/cpm -d curry_bin=$(REPL) test
