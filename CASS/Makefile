# Make to generate the Curry Analysis Server System
# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

# directories
ANADIR = ../analysis

# binary
TOOL   = $(BINDIR)/$(CURRYSYSTEM)-analyze

# Source modules of CASS:
DEPS = *.curry \
       $(LIBDIR)/Distribution.curry $(LIBDIR)/PropertyFile.curry \
       $(LIBDIR)/FlatCurry/*.curry $(LIBDIR)/SCC.curry $(ANADIR)/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: cass_worker cass

install: compile
	rm -f $(TOOL) $(BINDIR)/cass
	cd $(BINDIR) && ln -s ../currytools/CASS/cass $(notdir $(TOOL))

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
	$(REPL) $(REPL_OPTS) :set path $(ANADIR) :l Main :save :q
	mv Main $@

# load the analysis server program into the Curry system:
.PHONY: load
load: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(ANADIR) :l AnalysisServer
