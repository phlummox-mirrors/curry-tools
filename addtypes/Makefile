# Makefile for generating AddTypes tool

TOOL = $(BINDIR)/$(CURRYSYSTEM)-addtypes

.PHONY: all compile install clean uninstall

all: install

compile: AddTypes

install: compile
	rm -f $(TOOL) $(BINDIR)/addtypes
	ln -s $(CURDIR)/AddTypes $(TOOL)
	ln -s $(TOOL) $(BINDIR)/addtypes # for backward compatibility

clean:
	$(CLEANCURRY)
	rm -f AddTypes

uninstall: clean
	rm -f $(TOOL)

AddTypes: AddTypes.curry $(LIBDIR)/AbstractCurry/*.curry \
	  $(LIBDIR)/List.curry $(LIBDIR)/AllSolutions.curry
	$(REPL) $(REPL_OPTS) :load AddTypes :save :quit


