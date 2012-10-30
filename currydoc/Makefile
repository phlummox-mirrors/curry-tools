# Makefile for generating CurryDoc tool

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta
ANADIR=../analysis

.PHONY: all
all:
	$(MAKE) CurryDoc4$(CURRYSYSTEM)

.PHONY: CurryDoc4pakcs
CurryDoc4pakcs:
	$(MAKE) CurryDoc.state

.PHONY: CurryDoc4kics2
CurryDoc4kics2:
	$(MAKE) CurryDoc

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
CurryDoc.state: $(DEPS)
	(echo ":set path $(ANADIR)" ; echo ":l CurryDoc" ; echo ":set path" ; \
	 echo ":save main" ) | $(ROOT)/bin/pakcs
	(cd $(ROOT)/bin ; rm -f currydoc ; \
	                  ln -s ../currytools/currydoc/CurryDoc.state currydoc)

# generate KiCS2 executable for currydoc program:
CurryDoc: $(DEPS)
	$(ROOT)/bin/kics2 $(REPL_OPTS) :set path $(ANADIR) :l CurryDoc :save :q
	(cd $(ROOT)/bin ; rm -f currydoc ; \
	                  ln -s ../currytools/currydoc/CurryDoc currydoc)

.PHONY: clean
clean: 
	$(ROOT)/bin/cleancurry
	rm -f $(ROOT)/bin/currydoc
