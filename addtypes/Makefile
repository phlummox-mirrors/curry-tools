# Makefile for generating AddTypes tool

TOOL = $(BINDIR)/addtypes

.PHONY: all compile install clean uninstall

all: install

compile: AddTypes

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/AddTypes $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f AddTypes

uninstall: clean
	rm -f $(TOOL)

AddTypes: AddTypes.curry $(METADIR)/AbstractCurry.curry \
	  $(LIBDIR)/List.curry $(LIBDIR)/AllSolutions.curry
	$(REPL) $(REPL_OPTS) :load AddTypes :save :quit


