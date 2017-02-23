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


########################################################################
# Testing the preprocessor

# Directories with test suites for the preprocessor:
TESTTOOLS = IntegratedCode.Examples IntegratedCode.ExamplesSQL \
            DefaultRules.Examples ContractWrapper.Examples

# run the test suites to check the tools
.PHONY: runtest
runtest: $(addprefix runtest_,$(TESTTOOLS))

runtest_%:
	cd $(subst .,/,$*) && ./test.sh $(RUNTESTPARAMS)
