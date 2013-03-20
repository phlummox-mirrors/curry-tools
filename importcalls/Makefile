# Makefile for generating ImportCalls tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta

# generate executable for import calls tools:
ImportCalls: ImportCalls.curry $(META)/FlatCurry.curry \
	     $(LIB)/System.curry $(LIB)/List.curry
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) :l ImportCalls :save :q

.PHONY: clean
clean: 
	$(ROOT)/bin/cleancurry
	rm -f ImportCalls
