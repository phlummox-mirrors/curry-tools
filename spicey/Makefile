##############################################################################
# Makefile for Spicey Web Framework
##############################################################################

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

# binaries
TOOL = $(ROOT)/bin/spiceup

# The directory containg the project generator
PROJECT_GEN_DIR=project_generator

# Some modules required by the Spicey implementation:
DEPS = $(PROJECT_GEN_DIR)/SpiceUp.curry scaffolding/*.curry \
       $(LIBDIR)/AbstractCurry/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: spiceup

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/spiceup $(TOOL)

clean:
	$(CLEANCURRY) -r
	rm -f spiceup

uninstall: clean
	rm -f $(TOOL)

# generate executable for Spicey generator:
spiceup: $(DEPS)
	CURRYPATH=scaffolding && export CURRYPATH && \
	cd $(PROJECT_GEN_DIR) && \
	  $(REPL) $(REPL_OPTS) :set path ../scaffolding:../../erd2curry \
	    :load SpiceUp :save "main \"$(CURDIR)/$(PROJECT_GEN_DIR)\"" :quit
	mv $(PROJECT_GEN_DIR)/SpiceUp $@
