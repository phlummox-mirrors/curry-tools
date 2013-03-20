# Makefile for generating CurryDoc tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

# directories
LIBDIR     = $(ROOT)/lib
METADIR    = $(LIBDIR)/meta
CASS       = ../CASS
ANADIR     = ../analysis

# binaries
REPL       = $(ROOT)/bin/$(CURRYSYSTEM)
CLEANCURRY = $(ROOT)/bin/cleancurry
INSTALLBIN = $(ROOT)/bin/currydoc

# Source modules of currydoc:
DEPS = CurryDoc.curry CurryDocRead.curry CurryDocHtml.curry \
       CurryDocTeX.curry CurryDocParams.curry CurryDocConfig.curry \
       BootstrapStyle.curry \
       $(LIBDIR)/Markdown.curry \
       $(METADIR)/FlexRigid.curry \
       $(LIBDIR)/CategorizedHtmlList.curry \
       $(LIBDIR)/Distribution.curry $(LIBDIR)/PropertyFile.curry \
       $(METADIR)/FlatCurry.curry \
       $(LIBDIR)/HTML.curry $(LIBDIR)/HtmlParser.curry \
       $(CASS)/AnalysisServer.curry \
       $(ANADIR)/Deterministic.curry $(ANADIR)/TotallyDefined.curry \
       $(ANADIR)/Indeterministic.curry $(ANADIR)/SolutionCompleteness.curry

.PHONY: install
install: CurryDoc
	$(MAKE) uninstall
	ln -s $(CURDIR)/$< $(INSTALLBIN)

.PHONY: uninstall
uninstall:
	rm -f $(INSTALLBIN)

# generate executable for currydoc program:
CurryDoc: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(CASS):$(ANADIR) :load $@ :save :quit

.PHONY: clean
clean: uninstall
	$(CLEANCURRY)
	rm -f CurryDoc
