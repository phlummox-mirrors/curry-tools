# Makefile for the type inference tool
# No executable is generated but the modules are pre-compiled
# so that they can be included in other programs.

# pre-compiler type inference tool
.curry/Inference.fcy: Inference.curry $(wildcard *.curry)
	$(REPL) $(REPL_OPTS) :load Inference :quit

.PHONY: clean
clean:
	$(CLEANCURRY)
