# Makefile for the type inference tool
# No executable is generated but the modules are pre-compiled
# so that they can be included in other programs.

.PHONY: all compile install clean uninstall

all: compile install clean

compile: .curry/Inference.fcy

install: # nothing to do

clean: # nothing to do since we want to keep the pre-compiled programs

uninstall:
	$(CLEANCURRY)

.curry/Inference.fcy: $(wildcard *.curry)
	$(REPL) $(REPL_OPTS) :load Inference :quit
