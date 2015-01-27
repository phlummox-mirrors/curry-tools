# Makefile for generating currytest tool

TOOL = $(BINDIR)/currytest

.PHONY: all compile install clean uninstall

all: install

compile: CurryTest

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/CurryTest $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f CurryTest

uninstall: clean
	rm -f $(TOOL)

CurryTest:  CurryTest.curry         $(METADIR)/FlatCurry.curry   \
	  $(LIBDIR)/System.curry    $(LIBDIR)/Assertion.curry    \
	  $(LIBDIR)/Directory.curry $(LIBDIR)/Distribution.curry \
	  $(LIBDIR)/GUI.curry
	$(REPL) $(REPL_OPTS) :load CurryTest :save :quit


