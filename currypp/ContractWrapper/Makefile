########################################################################
# Makefile for contract wrapping tool
#
# This makefile is only used if the tool should be installed manually
# (for testing). Usually, it will be automatically with currypp
# via the ../Makefile (of currypp).
########################################################################

TOOLDIR = ../..

# additional imports for determinism analysis, for module ContractUsage,
# and rewriting libraries for contract optimization:
REWRITINGDIR  = $(CURDIR)/../../verification
PPPATH = $(TOOLDIR)/analysis:$(TOOLDIR)/CASS:$(TOOLDIR)/currycheck:$(REWRITINGDIR)

.PHONY: all
all: cwrapper

# generate saved state for contract wrapper:
cwrapper: TransContracts.curry SimplifyPostConds.curry
	curry :set path $(PPPATH) :l TransContracts :save :q && mv TransContracts cwrapper

.PHONY: clean
clean: 
	cleancurry -r
	rm -f cwrapper
	cd Examples && rm -f *C.curry *~

########################################################################
