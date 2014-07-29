# Makefile for generating Curry->JavaScript tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

TOOL = Curry2JS

# generate executable for the Curry2JavaScript translator
$(TOOL):  Curry2JS.curry $(LIBDIR)/JavaScript.curry \
	        $(METADIR)/CompactFlatCurry.curry $(METADIR)/FlatCurry.curry
	$(REPL) $(REPL_OPTS) :load Curry2JS :save :quit

clean:
	$(CLEANCURRY)
	rm -f $(TOOL)

