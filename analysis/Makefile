# Makefile for analysis files (used in CASS)

ANALYSES = $(wildcard *.curry)
ANA_FCY  = $(ANALYSES:%.curry=.curry/%.fcy)
CASS     = ../CASS

.PHONY: all compile install clean uninstall

all: install

compile:
	for a in $(ANA_FCY); do $(MAKE) $$a; done

install: compile

clean:
	$(CLEANCURRY)

uninstall: clean

# compile a source program:
.curry/%.fcy: %.curry $(CASS)/Analysis.curry $(CASS)/GenericProgInfo.curry
	"$(REPL)" $(REPL_OPTS) :set path $(CASS) :load $* :quit
