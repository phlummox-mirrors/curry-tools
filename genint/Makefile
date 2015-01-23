# Makefile for generating GenInt tool

TOOL = $(BINDIR)/GenInt

.PHONY: all compile install clean uninstall

all: compile install clean

compile: GenInt

install: GenInt
	mv GenInt $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f GenInt

uninstall:
	rm -f $(TOOL)

# generate executable for currydoc program:
GenInt: GenInt.curry $(METADIR)/FlatCurry.curry \
	  $(METADIR)/FlatCurryShow.curry $(METADIR)/FlexRigid.curry \
	  $(LIBDIR)/List.curry
	$(REPL) $(REPL_OPTS) :load GenInt :save :quit
