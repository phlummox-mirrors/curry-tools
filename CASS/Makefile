# Make to generate the Curry Analysis Server System
# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

# directories
ANADIR = ../analysis

# binary
TOOL   = $(BINDIR)/cass

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
       $(ANADIR)/Demandedness.curry $(ANADIR)/Groundness.curry \
       $(ANADIR)/RequiredValues.curry

.PHONY: all compile install clean uninstall

all: install

compile: cass_worker cass

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/cass $(TOOL)

clean:
	$(CLEANCURRY) -r
	rm -f cass_worker cass

uninstall: clean
	rm -f $(TOOL)

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
