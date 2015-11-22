# Makefile for generating GenInt tool

# binary
TOOL = $(BINDIR)/runcurry

.PHONY: all compile install clean uninstall

all: install

compile: RunCurry

install: compile # no further installation required
	rm -f $(TOOL)
	ln -s $(CURDIR)/RunCurry $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f RunCurry

uninstall: clean
	rm -f $(TOOL)

# generate executable for currydoc program:
RunCurry: RunCurry.curry
	$(REPL) $(REPL_OPTS) :load RunCurry :save :quit
