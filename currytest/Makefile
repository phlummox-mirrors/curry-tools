# Makefile for generating currytest tool

TOOL = $(BINDIR)/currytest

.PHONY: all compile install clean uninstall

all: compile install

compile: CurryTest

install: CurryTest uninstall
	ln -s $(CURDIR)/CurryTest $(TOOL)

clean: uninstall
	$(CLEANCURRY)
	rm -f CurryTest

uninstall:
	rm -f $(TOOL)

CurryTest:  CurryTest.curry         $(METADIR)/FlatCurry.curry   \
	  $(LIBDIR)/System.curry    $(LIBDIR)/Assertion.curry    \
	  $(LIBDIR)/Directory.curry $(LIBDIR)/Distribution.curry \
	  $(LIBDIR)/GUI.curry
	$(REPL) $(REPL_OPTS) :load CurryTest :save :quit


