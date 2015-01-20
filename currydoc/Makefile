# Makefile for generating CurryDoc tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

# directories
CASS       = ../CASS
ANADIR     = ../analysis
# binary
TOOL       = $(BINDIR)/currydoc

# Source modules of currydoc
DEPS = CurryDoc.curry CurryDocRead.curry CurryDocHtml.curry \
       CurryDocTeX.curry CurryDocCDoc.curry \
       CurryDocParams.curry CurryDocConfig.curry \
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

# generate executable for currydoc program
$(TOOL): $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(CASS):$(ANADIR) :load CurryDoc :save :quit
	mv CurryDoc $@

.PHONY: clean
clean:
	$(CLEANCURRY)
	rm -f CurryDoc $(TOOL)
