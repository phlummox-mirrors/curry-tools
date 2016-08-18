# Makefile for binding optimizer:

# directories
ANADIRS = ../../analysis:../../CASS

# binaries
# TODO: move to $(BINDIR)/.local
TOOL = $(ROOT)/currytools/optimize/bindingopt

.PHONY: all compile install clean uninstall

all: install

compile: BindingOpt

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/BindingOpt $(TOOL)

clean:
	$(CLEANCURRY) -r
	rm -f BindingOpt

uninstall: clean
	rm -f $(TOOL)

BindingOpt: BindingOpt.curry
	# create executable for top-level function "main":
	$(REPL) $(REPL_OPTS) :set path $(ANADIRS) :l $< :save :q

# Load only the program into the Curry system:
.PHONY: load
load: BindingOpt.curry
	$(REPL) $(REPL_OPTS) :set path $(ANADIRS) :l $<
