########################################################################
# Makefile for contract wrapping tool
# (only used for manual installation since it will be automatically
# installed via the ../Makefile of currypp)
########################################################################

TOOLDIR = ../..
# for determinism analysis and for module ContractUsage:
PPPATH = $(TOOLDIR)/analysis:$(TOOLDIR)/CASS:$(TOOLDIR)/currycheck

.PHONY: all
all: cwrapper

# generate saved state for contract wrapper:
cwrapper: TransContracts.curry
	pakcs :set path $(PPPATH) :l TransContracts :save :q && mv TransContracts cwrapper

.PHONY: clean
clean: 
	cleancurry -r
	rm -f cwrapper
	cd Examples && rm -f *C.curry *~

########################################################################
