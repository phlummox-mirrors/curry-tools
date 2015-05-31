#****************************************************************************
# A Code Integrator for Curry
#
# Contact:
# Max Deppert - made@informatik.uni-kiel.de
# Jasper Sikorra - jsi@informatik.uni-kiel.de
#
#****************************************************************************

#----------------------------------
# Input and output dirs
#----------------------------------

# Tool binary
TOOL = $(BINDIR)/currypp

# Source filepaths
SRCDIR  = $(CURDIR)
PARSEDIR = $(SRCDIR)/Parser
MLDIR = $(PARSEDIR)/ML

.PHONY: all compile install clean uninstall

all: install

compile: Main

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/Main $(TOOL)
	cd SequentialRules && $(MAKE) install

clean:
	$(CLEANCURRY) -r
	rm -f Translator
	cd SequentialRules && $(MAKE) clean

uninstall: clean
	rm -f $(TOOL)
	cd SequentialRules && $(MAKE) uninstall

# generate executable of currypp translator:
Main: $(SRCDIR)/*.curry $(PARSEDIR)/*.curry $(MLDIR)/*.curry
	@echo Compiling Curry Integrated Code Translator ...
	$(REPL) $(REPL_OPTS) :set path $(SRCDIR):$(PARSEDIR):$(MLDIR) \
	  :load Main :save :quit
