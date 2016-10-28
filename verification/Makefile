# Makefile for generating the curry2verify tool

TOOL=$(BINDIR)/$(CURRYSYSTEM)-verify

# for determinism analysis and for module TheoremUsage:
TOOLDIR=..
CCPATH = $(TOOLDIR)/currycheck
CVPATH = $(TOOLDIR)/analysis:$(TOOLDIR)/CASS:$(CCPATH)

DEPS = *.curry $(CCPATH)/PropertyUsage.curry

.PHONY: all compile install clean uninstall

all: install

compile: ToVerifier

install: ToVerifier
	rm -f $(TOOL) $(BINDIR)/curry2verify
	ln -s $(CURDIR)/ToVerifier $(TOOL)
	ln -s $(TOOL) $(BINDIR)/curry2verify # for backward compatibility

clean:
	$(CLEANCURRY)
	rm -f ToVerifier

uninstall: clean
	rm -f $(TOOL)

ToVerifier: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(CVPATH) :l ToVerifier :save :q
