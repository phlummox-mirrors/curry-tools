# Makefile for generating GenInt tool

TOOL = GenInt

.PHONY: all compile install clean uninstall

all: install

compile: GenInt

install: compile # no further installation required

clean:
	$(CLEANCURRY)
	rm -f GenInt

uninstall: clean

# generate executable for currydoc program:
GenInt: GenInt.curry $(METADIR)/FlatCurry.curry \
	  $(METADIR)/FlatCurryShow.curry $(METADIR)/FlexRigid.curry \
	  $(LIBDIR)/List.curry
	$(REPL) $(REPL_OPTS) :load GenInt :save :quit
