# Makefile for generating GenInt tool

TOOL = GenInt

# generate executable for currydoc program:
$(TOOL): GenInt.curry $(METADIR)/FlatCurry.curry \
	  $(METADIR)/FlatCurryShow.curry $(METADIR)/FlexRigid.curry \
	  $(LIBDIR)/List.curry
	$(REPL) $(REPL_OPTS) :load GenInt :save :quit

.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f $(TOOL)
