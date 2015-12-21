# Makefile for ERD->Curry compiler
#
# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - bin directory of the Curry System specified by variable BINDIR

# binaries
TOOL = $(BINDIR)/erd2curry

# Some modules required by the erd2curry implementation:
DEPS = ERD2Curry.curry CodeGeneration.curry Transformation.curry \
       XML2ERD.curry ERDGoodies.curry ERD2Graph.curry \
       ERD.curry $(LIBDIR)/AbstractCurry/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: erd2curry

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/erd2curry $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f erd2curry

uninstall: clean
	rm -f $(TOOL)

erd2curry: $(DEPS)
	# create saved state for top-level function "main":
	$(REPL) $(REPL_OPTS) :load ERD2Curry :save "main \"$(CURDIR)\"" :q
	mv ERD2Curry $@
