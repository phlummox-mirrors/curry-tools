# Makefile for binding optimizer:

# Redefine this variable according to your Curry installation:
CURRYHOME=$(HOME)/pakcs

# directories
ANADIRS=../../analysis:../../CASS

# binaries
REPL       = $(ROOT)/bin/curry
CLEANCURRY = $(ROOT)/bin/cleancurry
INSTALLBIN = $(ROOT)/currytools/optimize/bindingopt

.PHONY: install
install: BindingOpt
	$(MAKE) uninstall
	ln -s $(CURDIR)/BindingOpt $(INSTALLBIN)

.PHONY: uninstall
uninstall:
	rm -f $(INSTALLBIN)

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
	$(CURRYHOME)/bin/cleancurry -r
	cd examples && $(INSTALLBIN) -f $(TESTPROGS) | tee test.log
	cd examples && meld TEST.log test.log 

.PHONY: selftest
selftest:
	export CURRYPATH=$(ANADIRS) && ./eqopt -f BindingOpt.curry

.PHONY: clean
clean: uninstall
	$(CLEANCURRY) -r
	rm -f BindingOpt
