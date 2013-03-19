# Makefile for generating GenInt tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta

# Source modules of currydoc:
DEPS = GenInt.curry $(META)/FlatCurry.curry \
       $(META)/FlatCurryShow.curry $(META)/FlexRigid.curry \
       $(LIB)/List.curry

# generate executable for currydoc program:
GenInt: $(DEPS)
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) :l GenInt :save :q

.PHONY: clean
clean: 
	$(ROOT)/bin/cleancurry
	rm -f GenInt
