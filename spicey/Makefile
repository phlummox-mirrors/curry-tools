##############################################################################
# Makefile for Spicey Web Framework
##############################################################################

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

# binaries
REPL       = $(ROOT)/bin/$(CURRYSYSTEM)
CLEANCURRY = $(ROOT)/bin/cleancurry
INSTALLBIN = $(ROOT)/bin/spiceup

# The directory containg the project generator
PROJECT_GEN_DIR=project_generator

# Some modules required by the Spicey implementation:
DEPS = $(PROJECT_GEN_DIR)/SpiceUp.curry scaffolding/*.curry

.PHONY: install
install: spiceup
	$(MAKE) uninstall
	ln -s $(CURDIR)/spiceup $(INSTALLBIN)

.PHONY: uninstall
uninstall:
	rm -f $(INSTALLBIN)

# generate executable for Spicey generator:
spiceup: $(DEPS)
	CURRYPATH=scaffolding && export CURRYPATH && \
	cd $(PROJECT_GEN_DIR) && \
	  $(REPL) $(REPL_OPTS) :set path ../scaffolding:../../erd2curry \
	    :load SpiceUp :save "main \"$(CURDIR)/$(PROJECT_GEN_DIR)\"" :quit
	mv $(PROJECT_GEN_DIR)/SpiceUp $@

.PHONY: clean
clean: uninstall
	$(CLEANCURRY)
	rm -f spiceup
