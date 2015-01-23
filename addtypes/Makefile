# Makefile for generating AddTypes tool

TOOL = $(BINDIR)/addtypes

.PHONY: all compile install clean uninstall

all: compile install clean

compile: AddTypes

install: AddTypes
	mv AddTypes $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f AddTypes

uninstall:
	rm -f $(TOOL)

AddTypes: AddTypes.curry $(METADIR)/AbstractCurry.curry \
	  $(LIBDIR)/List.curry $(LIBDIR)/AllSolutions.curry
	$(REPL) $(REPL_OPTS) :load AddTypes :save :quit


