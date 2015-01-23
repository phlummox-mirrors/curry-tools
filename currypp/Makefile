#****************************************************************************
# A Code Integrator for Curry
#
# Contact:
# Max Deppert - made@informatik.uni-kiel.de
# Jasper Sikorra - jsi@informatik.uni-kiel.de
#
#****************************************************************************

# The major version numbers:
MAJORVERSION=1

# The minor version number:
MINORVERSION=8

# The revision version number:
REVISIONVERSION=2

# Complete version:
VERSION=$(MAJORVERSION).$(MINORVERSION).$(REVISIONVERSION)

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

all: compile install clean

compile: Translator

install: Translator
	rm -f $(TOOL)
	ln -s Translator $(TOOL)

clean:
	$(CLEANCURRY) -r
	rm -f Translator

uninstall:
	rm -f $(TOOL)

# generate executable of currypp translator:
Translator: $(MAIN) $(PARSEDIR)/*.curry $(MLDIR)/*.curry
	@echo Compiling Curry Integrated Code Translator ...
	$(REPL) $(REPL_OPTS) :set path $(SRCDIR):$(PARSEDIR):$(MLDIR) \
	  :load $(MAIN) :save :quit
	@echo Compilation finished ...
	@echo Version: $(VERSION)
