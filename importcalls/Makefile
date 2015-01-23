# Makefile for generating ImportCalls tool

TOOL = $(BINDIR)/ImportCalls

.PHONY: all compile install clean uninstall

all: compile install clean

compile: ImportCalls

install: ImportCalls

clean:
	$(CLEANCURRY)
	rm -f ImportCalls

uninstall:
	rm -f $(TOOL)

# generate executable for import calls tools:
ImportCalls: ImportCalls.curry $(METADIR)/FlatCurry.curry \
	     $(LIBDIR)/System.curry $(LIBDIR)/List.curry
	$(REPL) $(REPL_OPTS) :load ImportCalls :save :quit
