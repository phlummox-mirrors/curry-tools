# Makefile for generating CurryDoc tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta
ANADIR=../analysis

# Source modules of currydoc:
DEPS = CurryDoc.curry CurryDocRead.curry CurryDocHtml.curry \
       CurryDocTeX.curry CurryDocParams.curry \
       $(LIB)/Markdown.curry \
       $(META)/FlexRigid.curry \
       $(LIB)/CategorizedHtmlList.curry \
       $(LIB)/Distribution.curry $(LIB)/PropertyFile.curry \
       $(META)/FlatCurry.curry \
       $(LIB)/HTML.curry $(LIB)/HtmlParser.curry \
       $(ANADIR)/AnaOverlapping.curry $(ANADIR)/AnaIndeterminism.curry \
       $(ANADIR)/AnaCompleteness.curry $(ANADIR)/AnaOpComplete.curry \
       $(ANADIR)/AnaDependency.curry 

# generate saved PAKCS state for currydoc program:
CurryDoc: $(DEPS)
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) :set path $(ANADIR) \
	                           :l CurryDoc :save :q
	(cd $(ROOT)/bin ; rm -f currydoc ; \
	                  ln -s ../currytools/currydoc/CurryDoc currydoc)

.PHONY: clean
clean: 
	$(ROOT)/bin/cleancurry
	rm -f $(ROOT)/bin/currydoc
