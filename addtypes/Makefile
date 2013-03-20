# Makefile for generating ImportCalls tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta

# generate executable for AddTypes tool:
AddTypes: AddTypes.curry $(META)/AbstractCurry.curry \
	     $(LIB)/List.curry $(LIB)/AllSolutions.curry
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) :l AddTypes :save :q
	(cd $(ROOT)/bin ; rm -f addtypes ; \
	                  ln -s ../currytools/addtypes/AddTypes addtypes)

.PHONY: clean
clean: 
	$(ROOT)/bin/cleancurry
	rm -f AddTypes $(ROOT)/bin/addtypes
