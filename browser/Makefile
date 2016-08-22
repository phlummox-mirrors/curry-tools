# Makefile for generating Curry Browser

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

CURRYTOOLS = ..
CASS       = $(CURRYTOOLS)/CASS
ANADIR     = $(CURRYTOOLS)/analysis

# binary
TOOL       = $(BINDIR)/currybrowse

# source modules of the Curry Browser:
DEPS = BrowserGUI.curry Imports.curry \
       AnalysisTypes.curry BrowserAnalysis.curry ShowGraph.curry \
       $(CASS)/AnalysisServer.curry $(CASS)/AnalysisDoc.curry \
       $(CASS)/Registry.curry \
       $(CURRYTOOLS)/addtypes/AddTypes.curry \
       $(CURRYTOOLS)/genint/GenInt.curry \
       $(CURRYTOOLS)/importcalls/ImportCalls.curry \
       $(LIBDIR)/GUI.curry $(LIBDIR)/IOExts.curry  $(LIBDIR)/System.curry \
       $(LIBDIR)/FlatCurry/Types.curry $(LIBDIR)/FlatCurry/Files.curry \
       $(LIBDIR)/FlatCurry/Show.curry \
       analysis/*.curry $(ANADIR)/*.curry

LOADPATH = analysis:$(CASS):$(ANADIR):$(CURRYTOOLS)/genint:$(CURRYTOOLS)/importcalls:$(CURRYTOOLS)/addtypes

.PHONY: all compile install clean uninstall

all: install

compile: BrowserGUI SourceProgGUI

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/BrowserGUI $(TOOL)

clean:
	$(CLEANCURRY) -r
	rm -f BrowserGUI SourceProgGUI

uninstall: clean
	rm -f $(TOOL)

# generate executable for Curry Browser:
BrowserGUI: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(LOADPATH) \
	  :load BrowserGUI :eval "patchReadmeVersion" :save :quit

SourceProgGUI: SourceProgGUI.curry $(LIBDIR)/GUI.curry
	$(REPL) $(REPL_OPTS) :load SourceProgGUI :save :quit

# load the Curry Browser program into the Curry system:
.PHONY: load
load: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(LOADPATH) :load BrowserGUI
