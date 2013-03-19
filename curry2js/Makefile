# Makefile for generating Curry->JavaScript tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta

# generate executable for the Curry2JavaScript translator:
Curry2JS: Curry2JS.curry $(LIB)/JavaScript.curry \
	        $(META)/CompactFlatCurry.curry $(META)/FlatCurry.curry
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) :l Curry2JS :save :q

clean: 
	$(ROOT)/bin/cleancurry
	rm -f Curry2JS

