# Makefile for generating Curry->JavaScript tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

TOOL = Curry2JS

.PHONY: all compile install clean uninstall

all: compile install clean

compile: Curry2JS

install: Curry2JS

clean:
	$(CLEANCURRY) -r

uninstall:
	rm -f $(TOOL)

# generate executable for the Curry2JavaScript translator
Curry2JS:  Curry2JS.curry $(LIBDIR)/JavaScript.curry \
	        $(METADIR)/CompactFlatCurry.curry $(METADIR)/FlatCurry.curry
	$(REPL) $(REPL_OPTS) :load Curry2JS :save :quit
