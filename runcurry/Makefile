# Makefile for generating GenInt tool

# binary
TOOL = $(BINDIR)/$(CURRYSYSTEM)-run

.PHONY: all compile install clean uninstall

all: install

compile: RunCurry

install: compile # no further installation required
	rm -f $(TOOL) $(BINDIR)/runcurry
	ln -s $(CURDIR)/RunCurry $(TOOL)
	ln -s $(TOOL) $(BINDIR)/runcurry # for backward compatibility

clean:
	$(CLEANCURRY)
	rm -f RunCurry

uninstall: clean
	rm -f $(TOOL)

# generate executable for currydoc program:
RunCurry: RunCurry.curry
	$(REPL) $(REPL_OPTS) :load RunCurry :save :quit
