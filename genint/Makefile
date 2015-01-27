# Makefile for generating GenInt tool

TOOL = $(BINDIR)/GenInt

.PHONY: all compile install clean uninstall

all: install

compile: GenInt

install: compile
	rm -f $(TOOL)
	ln -s GenInt $(TOOL)

clean: uninstall
	$(CLEANCURRY)
	rm -f GenInt

uninstall:
	rm -f $(TOOL)

# generate executable for currydoc program:
GenInt: GenInt.curry $(METADIR)/FlatCurry.curry \
	  $(METADIR)/FlatCurryShow.curry $(METADIR)/FlexRigid.curry \
	  $(LIBDIR)/List.curry
	$(REPL) $(REPL_OPTS) :load GenInt :save :quit
