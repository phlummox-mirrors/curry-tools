# Makefile for cleaning analysis files

ANALYSES = $(wildcard *.curry)
CASS     = ../CASS

.PHONY: all compile install clean uninstall

all: compile install clean

compile:
	for a in $(ANALYSES); do $(REPL) $(REPL_OPTS) :set path $(CASS) :load $$a :quit; done

install: # nothing to do

clean:
	$(CLEANCURRY)

uninstall: # nothing to do
