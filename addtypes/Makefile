# Makefile for generating AddTypes tool

TOOL=$(BINDIR)/addtypes

$(TOOL):  AddTypes.curry       $(METADIR)/AbstractCurry.curry \
	  $(LIBDIR)/List.curry $(LIBDIR)/AllSolutions.curry
	$(REPL) $(REPL_OPTS) :load AddTypes :save :quit
	mv AddTypes $@

.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f AddTypes $(TOOL)
