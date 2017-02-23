# Makefile for generating GenInt tool

# binary
TOOL = $(BINDIR)/$(CURRYSYSTEM)-run

.PHONY: all compile install clean uninstall runtest

all: install

compile: RunCurry

install: compile # no further installation required
	rm -f $(TOOL) $(BINDIR)/runcurry
	cd $(BINDIR) && ln -s ../currytools/runcurry/RunCurry $(notdir $(TOOL))
	cd $(BINDIR) && ln -s $(notdir $(TOOL)) runcurry # for backward compatibility

clean:
	$(CLEANCURRY)
	rm -f RunCurry

uninstall: clean
	rm -f $(TOOL)
	rm -f $(BINDIR)/runcurry # for backward compatibility

# generate executable for currydoc program:
RunCurry: RunCurry.curry
	$(REPL) $(REPL_OPTS) :load RunCurry :save :quit

# run the test suite
runtest:
	cd Examples && ./test.sh $(RUNTESTPARAMS)
