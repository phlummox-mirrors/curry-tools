# Makefile for generating ImportCalls tool

TOOL = ImportCalls

# generate executable for import calls tools:
$(TOOL): ImportCalls.curry $(METADIR)/FlatCurry.curry \
	     $(LIBDIR)/System.curry $(LIBDIR)/List.curry
	$(REPL) $(REPL_OPTS) :load ImportCalls :save :quit

.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f $(TOOL)
