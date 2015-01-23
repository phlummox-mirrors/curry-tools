# Makefile for binding optimizer:

# directories
ANADIRS = ../../analysis:../../CASS

# binaries
# TODO: move to $(BINDIR)
TOOL = ../bindingopt

.PHONY: all compile install clean uninstall

all: compile install clean

compile: BindingOpt

install: BindingOpt
	rm -f $(TOOL)
	ln -s BindingOpt $(TOOL)

clean:
	$(CLEANCURRY) -r
	rm -f BindingOpt

uninstall:
	rm -f $(TOOL)

BindingOpt: BindingOpt.curry
	# create executable for top-level function "main":
	$(REPL) $(REPL_OPTS) :set path $(ANADIRS) :l $< :save :q

# Load only the program into the Curry system:
.PHONY: load
load: BindingOpt.curry
	$(REPL) $(REPL_OPTS) :set path $(ANADIRS) :l $<

TESTPROGS = HTML SetFunctions Parser AddTypes CurryStringClassifier Curry2JS

.PHONY: test
test:
	$(CLEANCURRY) -r
	cd examples && $(TOOL) -f $(TESTPROGS) | tee test.log
	cd examples && meld TEST.log test.log

.PHONY: selftest
selftest:
	export CURRYPATH=$(ANADIRS) && ./eqopt -f BindingOpt.curry
