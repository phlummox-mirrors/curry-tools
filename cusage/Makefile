##############################################################################
# Makefile for Spicey Web Framework
##############################################################################

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

# binaries
TOOL = $(ROOT)/bin/cusage

# Some modules required by the Spicey implementation:
DEPS = CheckUsage.curry AbstractCurryMatch.curry FlatCurryMatch.curry \
       $(LIBDIR)/AbstractCurry/*.curry $(LIBDIR)/FlatCurry/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: CheckUsage

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/CheckUsage $(TOOL)

clean:
	$(CLEANCURRY) -r
	rm -f CheckUsage

uninstall: clean
	rm -f $(TOOL)

# generate executable for Spicey generator:
CheckUsage: $(DEPS)
	$(REPL) $(REPL_OPTS) :load CheckUsage :save :quit
