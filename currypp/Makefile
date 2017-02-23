#****************************************************************************
# A Code Integrator for Curry
#
# Contact:
# Max Deppert - made@informatik.uni-kiel.de
# Jasper Sikorra - jsi@informatik.uni-kiel.de
#
#****************************************************************************
# Makefile for Curry preprocessor
#****************************************************************************

# Tool binary
TOOL = $(BINDIR)/$(CURRYSYSTEM)-pp

# Source filepaths
ICODEDIR      = $(CURDIR)/IntegratedCode
PARSEDIR      = $(ICODEDIR)/Parser
MLDIR         = $(PARSEDIR)/ML
SQLDIR        = $(PARSEDIR)/SQL
DEFRULESDIR   = $(CURDIR)/DefaultRules
CONTRACTDIR   = $(CURDIR)/ContractWrapper
REWRITINGDIR  = $(CURDIR)/../verification
# for modules ContractUsage and DefaultRuleUsage:
CURRYCHECKDIR = $(CURDIR)/../currycheck
# for determinism analysis used in contract wrapper:
DETANAPATH    = ../analysis:../CASS

# preprocessor input path and dependencies:
PPPATH = $(ICODEDIR):$(PARSEDIR):$(MLDIR):$(SQLDIR):$(DEFRULESDIR):$(CONTRACTDIR):$(REWRITINGDIR):$(CURRYCHECKDIR):$(DETANAPATH)
DEPS   = Main.curry $(ICODEDIR)/*.curry $(PARSEDIR)/*.curry $(MLDIR)/*.curry \
          $(SQLDIR)/*.curry $(DEFRULESDIR)/*.curry \
	  $(CONTRACTDIR)/*.curry $(CURRYCHECKDIR)/ContractUsage.curry \
	  $(CURRYCHECKDIR)/DefaultRuleUsage.curry \
	  $(CURRYCHECKDIR)/TheoremUsage.curry \
	  $(LIBDIR)/AbstractCurry/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: Main

install: compile
	rm -f $(TOOL) $(BINDIR)/currypp
	cd $(BINDIR) && ln -s ../currytools/currypp/Main $(notdir $(TOOL))
	cd $(BINDIR) && ln -s $(notdir $(TOOL)) currypp # for backward compatibility

clean:
	$(CLEANCURRY) -r
	rm -f Main

uninstall: clean
	rm -f $(TOOL)
	rm -f $(BINDIR)/currypp # for backward compatibility

# generate executable of currypp translator:
Main: $(DEPS)
	@echo Compiling Curry Preprocessor...
	$(REPL) $(REPL_OPTS) :set path $(PPPATH) :load Main :save :quit

# run the test suites for the Curry preprocessor
.PHONY: runtest
runtest:
	cd IntegratedCode/Examples    && ./test.sh $(RUNTESTPARAMS)
	cd IntegratedCode/ExamplesSQL && ./test.sh $(RUNTESTPARAMS)
	cd DefaultRules/Examples      && ./test.sh $(RUNTESTPARAMS)
	cd ContractWrapper/Examples   && ./test.sh $(RUNTESTPARAMS)
