# Makefile for generating AddTypes tool

TOOL = $(BINDIR)/addtypes

.PHONY: all compile install clean uninstall

all: compile install

compile: AddTypes

install: AddTypes uninstall
	ln -s $(CURDIR)/AddTypes $(TOOL)

clean: uninstall
	$(CLEANCURRY)
	rm -f AddTypes

uninstall:
	rm -f $(TOOL)

AddTypes: AddTypes.curry $(METADIR)/AbstractCurry.curry \
	  $(LIBDIR)/List.curry $(LIBDIR)/AllSolutions.curry
	$(REPL) $(REPL_OPTS) :load AddTypes :save :quit


