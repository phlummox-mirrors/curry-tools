# Makefile for ERD->Curry compiler
#
# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

# binaries
TOOL = $(BINDIR)/erd2curry

# Some modules required by the erd2curry implementation:
DEPS = ERD2Curry.curry CodeGeneration.curry Transformation.curry \
       XML2ERD.curry ERDGoodies.curry ERD2Graph.curry \
       ERD.curry

.PHONY: all compile install clean uninstall

all: compile install clean

compile: erd2curry

install: erd2curry
	rm -f $(TOOL)
	ln -s erd2curry $(TOOL)

clean: uninstall
	$(CLEANCURRY)
	rm -f erd2curry

uninstall:
	rm -f $(TOOL)

erd2curry: $(DEPS)
	# create saved state for top-level function "main":
	$(REPL) $(REPL_OPTS) :load ERD2Curry :save "main \"$(CURDIR)\"" :q
	mv ERD2Curry $@
