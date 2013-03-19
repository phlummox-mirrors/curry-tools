# Makefile for generating ImportCalls tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta

# generate executable for CurryTest tool:
CurryTest: CurryTest.curry $(META)/FlatCurry.curry \
	         $(LIB)/System.curry $(LIB)/Assertion.curry \
	         $(LIB)/Directory.curry $(LIB)/Distribution.curry \
	         $(LIB)/GUI.curry
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) :l CurryTest :save :q
	(cd $(ROOT)/bin ; rm -f currytest ; \
	                  ln -s ../currytools/currytest/CurryTest currytest)

.PHONY: clean
clean: 
	$(ROOT)/bin/cleancurry
	rm -f CurryTest $(ROOT)/bin/currytest
