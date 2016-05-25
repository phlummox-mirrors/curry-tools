# Makefile for generating the curry2verify tool

TOOL=$(BINDIR)/curry2verify

# for determinism analysis and for module TheoremUsage:
TOOLDIR=..
CVPATH = $(TOOLDIR)/analysis:$(TOOLDIR)/CASS:$(TOOLDIR)/currycheck

DEPS = *.curry

.PHONY: all compile install clean uninstall

all: install

compile: ToVerifier

install: ToVerifier
	rm -f $(TOOL)
	ln -s $(CURDIR)/ToVerifier $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f ToVerifier

uninstall: clean
	rm -f $(TOOL)

ToVerifier: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(CVPATH) :l ToVerifier :save :q
