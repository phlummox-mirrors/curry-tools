# Makefile for tool "CreateMakefile"

TOOL = $(BINDIR)/currycreatemake

.PHONY: all compile install clean uninstall

all: install

compile: CreateMakefile

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/CreateMakefile $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f CreateMakefile

uninstall: clean
	rm -f $(TOOL)

# generate executable for CreateMakefile program
CreateMakefile:  CreateMakefile.curry \
	  $(LIBDIR)/FlatCurry/Types.curry \
	  $(LIBDIR)/FlatCurry/Files.curry \
	  $(LIBDIR)/FlatCurry/Read.curry \
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
