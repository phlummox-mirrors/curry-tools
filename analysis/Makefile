# Makefile for analysis files (used in CASS)

ANALYSES = $(wildcard *.curry)
ANA_FCY  = $(ANALYSES:%.curry=.curry/%.fcy)
CASS     = ../CASS

empty     :=
space     := $(empty) $(empty)
# make_seq "a b c" = "make a && make b && make c"
make_seq = $(subst MAKE,$(MAKE) ,$(subst $(space), && ,$(addprefix MAKE,$(1))))

.PHONY: all compile install clean uninstall

all: install

compile:
	$(call make_seq,$(ANA_FCY))

install: compile

clean:
	$(CLEANCURRY)

uninstall: clean

# compile a source program:
.curry/%.fcy: %.curry $(CASS)/Analysis.curry $(CASS)/GenericProgInfo.curry
	"$(REPL)" $(REPL_OPTS) :set path $(CASS) :load $* :quit
