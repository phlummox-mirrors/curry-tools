# Makefile for generating Curry->JavaScript tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

TOOL =  Curry2JS

.PHONY: all compile install clean uninstall

all: install

compile: Curry2JS

install: compile # no further installation required

clean:
	$(CLEANCURRY) -r
	rm -f Curry2JS

uninstall: clean

# generate executable for the Curry2JavaScript translator
Curry2JS:  Curry2JS.curry $(LIBDIR)/JavaScript.curry \
	        $(LIBDIR)/FlatCurry/*.curry
	$(REPL) $(REPL_OPTS) :load Curry2JS :save :quit
