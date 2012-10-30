# Makefile for generating GenInt tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta

.PHONY: all
all:
	$(MAKE) GenInt4$(CURRYSYSTEM)

.PHONY: GenInt4pakcs
GenInt4pakcs:
	$(MAKE) GenInt.state

.PHONY: GenInt4kics2
GenInt4kics2:
	$(MAKE) GenInt

# Source modules of currydoc:
DEPS = GenInt.curry $(META)/FlatCurry.curry \
       $(META)/FlatCurryShow.curry $(META)/FlexRigid.curry \
       $(LIB)/List.curry

# generate saved PAKCS state for currydoc program:
GenInt.state: $(DEPS)
	$(ROOT)/bin/pakcs -s GenInt

# generate KiCS2 executable for currydoc program:
GenInt: $(DEPS)
	$(ROOT)/bin/kics2 $(REPL_OPTS) :l GenInt :save :q

.PHONY: clean
clean: 
	$(ROOT)/bin/cleancurry
