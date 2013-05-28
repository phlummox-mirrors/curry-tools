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

# binaries
REPL       = $(ROOT)/bin/$(CURRYSYSTEM)
CLEANCURRY = $(ROOT)/bin/cleancurry
INSTALLBIN = $(ROOT)/bin/currybrowse

# source modules of the Curry Browser:
DEPS = BrowserGUI.curry ShowFlatCurry.curry Imports.curry \
       AnalysisTypes.curry BrowserAnalysis.curry ShowGraph.curry \
       $(CASS)/AnalysisServer.curry $(CASS)/AnalysisCollection.curry \
       $(CURRYTOOLS)/addtypes/AddTypes.curry \
       $(CURRYTOOLS)/importcalls/ImportCalls.curry \
       $(LIB)/GUI.curry $(LIB)/IOExts.curry  $(LIB)/System.curry \
       $(META)/FlatCurry.curry $(META)/FlatCurryShow.curry \
       analysis/*.curry $(ANADIR)/*.curry

LOADPATH = analysis:$(CASS):$(ANADIR):$(CURRYTOOLS)/importcalls:$(CURRYTOOLS)/addtypes

.PHONY: install
install: BrowserGUI SourceProgGUI
	$(MAKE) uninstall
	ln -s $(CURDIR)/BrowserGUI $(INSTALLBIN)

.PHONY: uninstall
uninstall:
	rm -f $(INSTALLBIN)

# generate executable for Curry Browser:
BrowserGUI: $(DEPS)
	$(REPL) $(REPL_OPTS) \
	  :set path $(LOADPATH) \
	  :l BrowserGUI :eval "patchReadmeVersion" :save :q

# load the Curry Browser program into the Curry system:
.PHONY: load
load: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(LOADPATH) :l BrowserGUI

SourceProgGUI: SourceProgGUI.curry $(LIB)/GUI.curry
	$(ROOT)/bin/$(CURRYSYSTEM) $(REPL_OPTS) :l SourceProgGUI :save :q

.PHONY: clean
clean: uninstall
	$(CLEANCURRY) -r
	rm -f BrowserGUI SourceProgGUI
