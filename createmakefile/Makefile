# Makefile for tool "CreateMakefile":

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta

# generate executable for CreateMakefile program:
CreateMakefile: CreateMakefile.curry \
	  $(META)/FlatCurryRead.curry \
	  $(LIB)/FilePath.curry \
	  $(LIB)/PropertyFile.curry \
	  $(LIB)/ReadShowTerm.curry \
	  $(LIB)/Distribution.curry \
	  $(LIB)/FileGoodies.curry \
	  $(META)/FlatCurry.curry \
	  $(LIB)/Global.curry \
	  $(LIB)/List.curry \
	  $(LIB)/Maybe.curry \
	  $(LIB)/Directory.curry \
	  $(LIB)/IOExts.curry \
	  $(LIB)/Time.curry \
	  $(LIB)/Char.curry \
	  $(LIB)/IO.curry \
	  $(LIB)/System.curry
	# create saved state for top-level function "main":
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) :l CreateMakefile :save :q
	(cd $(ROOT)/bin ; rm -f currycreatemake ; \
	    ln -s ../currytools/createmakefile/CreateMakefile currycreatemake)

.PHONY: clean
clean:
	$(ROOT)/bin/cleancurry
	rm -f CreateMakefile $(ROOT)/bin/currycreatemake
