# Makefile for generating currytest tool

TOOL = $(BINDIR)/$(CURRYSYSTEM)-test

.PHONY: all compile install clean uninstall

all: install

compile: CurryTest

install: compile
	rm -f $(TOOL) $(BINDIR)/currytest
	ln -s $(CURDIR)/CurryTest $(TOOL)
	ln -s $(TOOL) $(BINDIR)/currytest # for backward compatibility

clean:
	$(CLEANCURRY)
	rm -f CurryTest

uninstall: clean
	rm -f $(TOOL)

CurryTest:  CurryTest.curry         $(LIBDIR)/FlatCurry/Files.curry   \
	  $(LIBDIR)/System.curry    $(LIBDIR)/Assertion.curry    \
	  $(LIBDIR)/Directory.curry $(LIBDIR)/Distribution.curry \
	  $(LIBDIR)/GUI.curry
	$(REPL) $(REPL_OPTS) :load CurryTest :save :quit


