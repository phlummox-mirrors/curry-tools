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
MAIN = $(SRCDIR)/Translator.curry

.PHONY: all compile install clean uninstall

all: install

compile: Translator

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/Translator $(TOOL)

clean:
	$(CLEANCURRY) -r
	rm -f Translator

uninstall: clean
	rm -f $(TOOL)

# generate executable of currypp translator:
Translator: $(MAIN) $(PARSEDIR)/*.curry $(MLDIR)/*.curry
	@echo Compiling Curry Integrated Code Translator ...
	$(REPL) $(REPL_OPTS) :set path $(SRCDIR):$(PARSEDIR):$(MLDIR) \
	  :load $(MAIN) :save :quit
