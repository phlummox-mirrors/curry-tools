# Makefile for generating currytest tool

TOOL = $(BINDIR)/currytest

$(TOOL):  CurryTest.curry           $(METADIR)/FlatCurry.curry   \
	  $(LIBDIR)/System.curry    $(LIBDIR)/Assertion.curry    \
	  $(LIBDIR)/Directory.curry $(LIBDIR)/Distribution.curry \
	  $(LIBDIR)/GUI.curry
	$(REPL) $(REPL_OPTS) :load CurryTest :save :quit
	mv CurryTest $@

.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f CurryTest $(TOOL)
