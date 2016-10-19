# Make to generate the Curry partial evaluator
# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

# binary
TOOL   = $(BINDIR)/peval

# Source modules of peval:
DEPS = *.curry

.PHONY: all compile install clean uninstall

all: install

compile: peval

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/peval $(TOOL)

clean:
	$(CLEANCURRY) -r
	rm -f peval

uninstall: clean
	rm -f $(TOOL)

# generate executable
peval: $(DEPS)
	$(REPL) $(REPL_OPTS) :load peval :save :quit
