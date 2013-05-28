# Make to generate the Curry Analysis Server System
# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

# directories
LIBDIR     = $(ROOT)/lib
METADIR    = $(LIBDIR)/meta
ANADIR     = ../analysis

# binaries
REPL       = $(ROOT)/bin/$(CURRYSYSTEM)
CLEANCURRY = $(ROOT)/bin/cleancurry
INSTALLBIN = $(ROOT)/bin/cass

# Source modules of CASS:
DEPS = Analysis.curry AnalysisCollection.curry AnalysisDependencies.curry \
       AnalysisServer.curry AnalysisWorker.curry Configuration.curry \
       CurryFiles.curry FlatCurryDependency.curry GenericProgInfo.curry \
       LoadAnalysis.curry SCC.curry ServerFormats.curry \
       ServerFunctions.curry WorkerFunctions.curry \
       $(LIBDIR)/Distribution.curry $(LIBDIR)/PropertyFile.curry \
       $(METADIR)/FlatCurry.curry \
       $(ANADIR)/Deterministic.curry $(ANADIR)/HigherOrder.curry \
       $(ANADIR)/Indeterministic.curry $(ANADIR)/RightLinearity.curry \
       $(ANADIR)/SolutionCompleteness.curry $(ANADIR)/TotallyDefined.curry \
       $(ANADIR)/Demandedness.curry $(ANADIR)/Groundness.curry

.PHONY: install
install: cass_worker cass
	$(MAKE) uninstall
	ln -s $(CURDIR)/cass $(INSTALLBIN)

.PHONY: uninstall
uninstall:
	rm -f $(INSTALLBIN)

# generate executable for analysis worker:
cass_worker: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(ANADIR) :l AnalysisWorker :save :q
	mv AnalysisWorker $@

# generate executable for analysis server program:
cass: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(ANADIR) :l AnalysisServer :save :q
	mv AnalysisServer $@

# load the analysis server program into the Curry system:
.PHONY: load
load: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(ANADIR) :l AnalysisServer

.PHONY: clean
clean: uninstall
	$(CLEANCURRY) -r
	rm -f cass_worker cass
