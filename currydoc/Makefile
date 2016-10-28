# Makefile for generating CurryDoc tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

# directories
CASS       = ../CASS
ANADIR     = ../analysis
# binary
TOOL       = $(BINDIR)/$(CURRYSYSTEM)-doc

# Source modules of currydoc
DEPS = CurryDoc.curry CurryDocRead.curry CurryDocAnaInfo.curry \
       CurryDocHtml.curry CurryDocTeX.curry CurryDocCDoc.curry \
       CurryDocParams.curry CurryDocConfig.curry \
       $(LIBDIR)/Markdown.curry \
       $(LIBDIR)/FlatCurry/FlexRigid.curry \
       $(LIBDIR)/CategorizedHtmlList.curry \
       $(LIBDIR)/Distribution.curry $(LIBDIR)/PropertyFile.curry \
       $(LIBDIR)/FlatCurry/Types.curry $(LIBDIR)/FlatCurry/Files.curry \
       $(LIBDIR)/HTML.curry $(LIBDIR)/HtmlParser.curry \
       $(LIBDIR)/Bootstrap3Style.curry \
       $(CASS)/AnalysisServer.curry \
       $(ANADIR)/Deterministic.curry $(ANADIR)/TotallyDefined.curry \
       $(ANADIR)/Indeterministic.curry $(ANADIR)/SolutionCompleteness.curry

.PHONY: all compile install clean uninstall

all: install

compile: CurryDoc

install: compile
	rm -f $(TOOL) $(BINDIR)/currydoc
	ln -s $(CURDIR)/CurryDoc $(TOOL)
	ln -s $(TOOL) $(BINDIR)/currydoc # for backward compatibility

clean:
	$(CLEANCURRY)
	rm -f CurryDoc

uninstall: clean
	rm -f $(TOOL)

# generate executable for currydoc program
CurryDoc: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(CASS):$(ANADIR) :load CurryDoc :save :quit
