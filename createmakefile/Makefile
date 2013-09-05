# Makefile for tool "CreateMakefile"

TOOL = $(BINDIR)/currycreatemake

# generate executable for CreateMakefile program
$(TOOL):  CreateMakefile.curry \
	  $(METADIR)/FlatCurry.curry $(METADIR)/FlatCurryRead.curry \
	  $(LIBDIR)/FilePath.curry \
	  $(LIBDIR)/PropertyFile.curry \
	  $(LIBDIR)/ReadShowTerm.curry \
	  $(LIBDIR)/Distribution.curry \
	  $(LIBDIR)/FileGoodies.curry \
	  $(LIBDIR)/Global.curry \
	  $(LIBDIR)/List.curry \
	  $(LIBDIR)/Maybe.curry \
	  $(LIBDIR)/Directory.curry \
	  $(LIBDIR)/IOExts.curry \
	  $(LIBDIR)/Time.curry \
	  $(LIBDIR)/Char.curry \
	  $(LIBDIR)/IO.curry \
	  $(LIBDIR)/System.curry
	# create saved state for top-level function "main":
	$(REPL) $(REPL_OPTS) :load CreateMakefile :save :quit
	mv CreateMakefile $@

.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f CreateMakefile $(TOOL)
