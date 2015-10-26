# Makefile for generating ImportCalls tool

TOOL = ImportCalls

.PHONY: all compile install clean uninstall

all: install

compile: ImportCalls

install: compile # no further installation required

clean:
	$(CLEANCURRY)
	rm -f ImportCalls

uninstall: clean

# generate executable for import calls tools:
ImportCalls: ImportCalls.curry $(LIBDIR)/FlatCurry/Types.curry \
	     $(LIBDIR)/FlatCurry/Files.curry \
	     $(LIBDIR)/System.curry $(LIBDIR)/List.curry
	$(REPL) $(REPL_OPTS) :load ImportCalls :save :quit
