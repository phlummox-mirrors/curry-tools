# Makefile for generating Curry Browser

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

TOOLS=$(ROOT)/tools
CURRYTOOLS=$(ROOT)/currytools
CASS=$(CURRYTOOLS)/CASS
ANADIR=$(CURRYTOOLS)/analysis

# binaries
TOOL = $(BINDIR)/currybrowse

# source modules of the Curry Browser:
DEPS = BrowserGUI.curry ShowFlatCurry.curry Imports.curry \
       AnalysisTypes.curry BrowserAnalysis.curry ShowGraph.curry \
       $(CASS)/AnalysisServer.curry $(CASS)/AnalysisCollection.curry \
       $(CURRYTOOLS)/addtypes/AddTypes.curry \
       $(CURRYTOOLS)/importcalls/ImportCalls.curry \
       $(LIBDIR)/GUI.curry $(LIBDIR)/IOExts.curry  $(LIBDIR)/System.curry \
       $(METADIR)/FlatCurry.curry $(METADIR)/FlatCurryShow.curry \
       analysis/*.curry $(ANADIR)/*.curry

LOADPATH = analysis:$(CASS):$(ANADIR):$(CURRYTOOLS)/importcalls:$(CURRYTOOLS)/addtypes

.PHONY: install
install: BrowserGUI SourceProgGUI
	$(MAKE) uninstall
	ln -s $(CURDIR)/BrowserGUI $(TOOL)

.PHONY: uninstall
uninstall:
	rm -f $(TOOL)

# generate executable for Curry Browser:
BrowserGUI: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(LOADPATH) \
	  :load BrowserGUI :eval "patchReadmeVersion" :save :quit

# load the Curry Browser program into the Curry system:
.PHONY: load
load: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(LOADPATH) :load BrowserGUI

SourceProgGUI: SourceProgGUI.curry $(LIBDIR)/GUI.curry
	$(REPL) $(REPL_OPTS) :load SourceProgGUI :save :quit

.PHONY: clean
clean: uninstall
	$(CLEANCURRY) -r
	rm -f BrowserGUI SourceProgGUI
