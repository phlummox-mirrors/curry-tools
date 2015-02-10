# Makefile for the type inference tool
# No executable is generated but the modules are pre-compiled
# so that they can be included in other programs.

.PHONY: all compile install clean uninstall

all: install

compile: .curry/Inference.fcy

install: compile

clean:
	$(CLEANCURRY)

uninstall: clean

.curry/Inference.fcy: $(wildcard *.curry)
	$(REPL) $(REPL_OPTS) :load Inference :quit
