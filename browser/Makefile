# Makefile for generating Curry Browser

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

LIB=$(ROOT)/lib
META=$(LIB)/meta
TOOLS=$(ROOT)/tools
CURRYTOOLS=$(ROOT)/currytools
CASS=$(CURRYTOOLS)/CASS
ANADIR=$(CURRYTOOLS)/analysis

.PHONY: all
all: BrowserGUI SourceProgGUI

# generate executable for Curry Browser:
BrowserGUI: BrowserGUI.curry ShowFlatCurry.curry Imports.curry \
	    AnalysisTypes.curry BrowserAnalysis.curry ShowGraph.curry \
	    $(CASS)/AnalysisServer.curry $(CASS)/AnalysisCollection.curry \
	    $(CURRYTOOLS)/addtypes/AddTypes.curry \
	    $(CURRYTOOLS)/importcalls/ImportCalls.curry \
	    $(LIB)/GUI.curry $(LIB)/IOExts.curry  $(LIB)/System.curry \
	    $(META)/FlatCurry.curry $(META)/FlatCurryShow.curry \
	    analysis/*.curry $(ANADIR)/*.curry
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) \
	  :set path analysis:$(CASS):$(ANADIR):$(CURRYTOOLS)/importcalls:$(CURRYTOOLS)/addtypes \
	  :l BrowserGUI :eval "patchReadmeVersion" :save :q
	(cd $(ROOT)/bin ; rm -f currybrowse ; \
	                  ln -s ../currytools/browser/BrowserGUI currybrowse)

SourceProgGUI: SourceProgGUI.curry $(LIB)/GUI.curry
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) :l SourceProgGUI :save :q

.PHONY: clean
clean: 
	$(ROOT)/bin/cleancurry
	rm -f BrowserGUI SourceProgGUI $(ROOT)/bin/currybrowse
