# Makefile for generating currytest tool

TOOL = $(BINDIR)/currytest

.PHONY: all compile install clean uninstall

all: compile install clean

compile: CurryTest

install: CurryTest
	rm -f $(TOOL)
	ln -s $(CURDIR)/CurryTest $(TOOL)

clean:
	$(CLEANCURRY)

uninstall:
	rm -f CurryTest $(TOOL)

CurryTest:  CurryTest.curry         $(METADIR)/FlatCurry.curry   \
	  $(LIBDIR)/System.curry    $(LIBDIR)/Assertion.curry    \
	  $(LIBDIR)/Directory.curry $(LIBDIR)/Distribution.curry \
	  $(LIBDIR)/GUI.curry
	$(REPL) $(REPL_OPTS) :load CurryTest :save :quit


