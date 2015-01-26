# Makefile for generating GenInt tool

TOOL = GenInt

.PHONY: all compile install clean uninstall

all: compile install

compile: GenInt

install: GenInt

clean: uninstall
	$(CLEANCURRY)

uninstall:
	rm -f $(TOOL)

# generate executable for currydoc program:
GenInt: GenInt.curry $(METADIR)/FlatCurry.curry \
	  $(METADIR)/FlatCurryShow.curry $(METADIR)/FlexRigid.curry \
	  $(LIBDIR)/List.curry
	$(REPL) $(REPL_OPTS) :load GenInt :save :quit
