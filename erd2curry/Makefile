# Makefile for ERD->Curry compiler
#
# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

# binaries
REPL       = $(ROOT)/bin/$(CURRYSYSTEM)
CLEANCURRY = $(ROOT)/bin/cleancurry
INSTALLBIN = $(ROOT)/bin/erd2curry

# Some modules required by the erd2curry implementation:
DEPS = ERD2Curry.curry CodeGeneration.curry Transformation.curry \
       XML2ERD.curry ERDGoodies.curry ERD2Graph.curry \
       ERD.curry

.PHONY: install
install: erd2curry
	$(MAKE) uninstall
	ln -s $(CURDIR)/erd2curry $(INSTALLBIN)

.PHONY: uninstall
uninstall:
	rm -f $(INSTALLBIN)

erd2curry: $(DEPS)
	# create saved state for top-level function "main":
	$(REPL) $(REPL_OPTS) :load ERD2Curry :save "main \"$(CURDIR)\"" :q
	mv ERD2Curry $@

.PHONY: clean
clean: uninstall
	$(CLEANCURRY)
	rm -f erd2curry
