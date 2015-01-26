# Makefile for generating AddTypes tool

TOOL = $(BINDIR)/addtypes

.PHONY: all compile install clean uninstall

all: compile install clean

compile: AddTypes

install: AddTypes
	rm -f $(TOOL)
	ln -s $(CURDIR)/AddTypes $(TOOL)

clean:
	$(CLEANCURRY)

uninstall:
	rm -f AddTypes $(TOOL)

AddTypes: AddTypes.curry $(METADIR)/AbstractCurry.curry \
	  $(LIBDIR)/List.curry $(LIBDIR)/AllSolutions.curry
	$(REPL) $(REPL_OPTS) :load AddTypes :save :quit


