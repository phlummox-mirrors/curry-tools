# Makefile for generating the Curry Automatic Style Checker

TOOL=$(BINDIR)/$(CURRYSYSTEM)-style

DEPS = *.curry AST/*.curry AutoCorr/*.curry Check/*.curry Config/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: CASC

install: CASC
	rm -f $(TOOL) $(BINDIR)/casc
	ln -s $(CURDIR)/CASC $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f CASC

uninstall: clean
	rm -f $(TOOL)

CASC: $(DEPS)
	$(REPL) $(REPL_OPTS) :l CASC :save :q
