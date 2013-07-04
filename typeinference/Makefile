# Makefile for the type inference tool
# No executable is generated but the modules are pre-compiled
# so that they can be included in other programs.

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

BIN=$(ROOT)/bin
LIB=$(ROOT)/lib
META=$(LIB)/meta

# pre-compiler type inference tool:
.curry/Inference.fcy: Inference.curry Unification.curry UnificationSpec.curry
	$(BIN)/$(CURRYSYSTEM) $(REPL_OPTS) :l Inference :q

.PHONY: clean
clean:
	$(BIN)/cleancurry
