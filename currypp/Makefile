#****************************************************************************
# A Code Integrator for Curry
#
# Contact:
# Max Deppert - made@informatik.uni-kiel.de
# Jasper Sikorra - jsi@informatik.uni-kiel.de
#
#****************************************************************************
# Makefile for Curry preprocessor
#****************************************************************************

# Tool binary
TOOL = $(BINDIR)/currypp

# Source filepaths
ICODEDIR    = $(CURDIR)/IntegratedCode
PARSEDIR    = $(ICODEDIR)/Parser
MLDIR       = $(PARSEDIR)/ML
SQLDIR      = $(PARSEDIR)/SQL
SEQRULESDIR = $(CURDIR)/SequentialRules
DEFRULESDIR = $(CURDIR)/DefaultRules

# preprocessor input path and dependencies:
PPPATH = $(ICODEDIR):$(PARSEDIR):$(MLDIR):$(SQLDIR):$(SEQRULESDIR):$(DEFRULESDIR)
DEPS   = Main.curry $(ICODEDIR)/*.curry $(PARSEDIR)/*.curry $(MLDIR)/*.curry \
          $(SQLDIR)/*.curry $(SEQRULESDIR)/*.curry $(DEFRULESDIR)/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: Main

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/Main $(TOOL)

clean:
	$(CLEANCURRY) -r

uninstall: clean
	rm -f $(TOOL)

# generate executable of currypp translator:
Main: $(DEPS)
	@echo Compiling Curry Preprocessor...
	$(REPL) $(REPL_OPTS) :set path $(PPPATH) :load Main :save :quit

# run the test suites for the Curry preprocessor
.PHONY: runtest
runtest:
	cd IntegratedCode/Examples && ./test.sh
	cd SequentialRules/Examples && ./test.sh
	cd DefaultRules/Examples && ./test.sh

