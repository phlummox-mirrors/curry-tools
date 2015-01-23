# Makefile for the type inference tool
# No executable is generated but the modules are pre-compiled
# so that they can be included in other programs.

.PHONY: all compile install clean uninstall

all: compile install clean

compile: Inference.curry $(wildcard *.curry)
	$(REPL) $(REPL_OPTS) :load Inference :quit

install: # nothing to do

clean:
	$(CLEANCURRY)

uninstall: # nothing to do
