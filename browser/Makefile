# Makefile for generating Curry Browser

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable REPL
# - root location of the Curry System specified by variable ROOT

# binary
TOOL       = $(BINDIR)/$(CURRYSYSTEM)-browse

# replacement stuff
empty     :=
space     := $(empty) $(empty)

# The load path of the browser:
LOADPATH = src:$(subst $(space),:,$(wildcard .cpm/packages/*/src))

# Source modules:
DEPS = .cpm/packages/*/src/*/*curry\
       src/*.curry src/*/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: BrowserGUI

install: compile
	rm -f $(TOOL)
	cd $(BINDIR) && ln -s ../currytools/browser/BrowserGUI $(notdir $(TOOL))

clean:
	$(CLEANCURRY) -r
	rm -f BrowserGUI

uninstall: clean
	rm -f $(TOOL)

# generate executable:
BrowserGUI: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(LOADPATH) :load BrowserGUI :save :quit
