# Makefile for cleaning analysis files

ANALYSES = $(wildcard *.curry)
CASS     = ../CASS

.PHONY: all compile install clean uninstall

all: install

compile:
	for a in $(ANALYSES); do $(REPL) $(REPL_OPTS) :set path $(CASS) :load $$a :quit; done

install: compile

clean:
	$(CLEANCURRY)

uninstall: clean
