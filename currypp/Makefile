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
SRCDIR      = $(CURDIR)
PARSEDIR    = $(SRCDIR)/Parser
MLDIR       = $(PARSEDIR)/ML
SEQRULESDIR = $(CURDIR)/SequentialRules
DEFRULESDIR = $(CURDIR)/DefaultRules

# preprocessor input path and dependencies:
PPPATH = $(SRCDIR):$(PARSEDIR):$(MLDIR):$(SEQRULESDIR):$(DEFRULESDIR)
DEPS   = $(SRCDIR)/*.curry $(PARSEDIR)/*.curry $(MLDIR)/*.curry \
         $(SEQRULESDIR)/*.curry $(DEFRULESDIR)/*.curry

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
