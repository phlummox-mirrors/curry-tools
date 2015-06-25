# Makefile for default rules preprocessor

# source modules of the default rules preprocessor:
DEPS = *.curry

TOOL=Transform

.PHONY: all compile install clean uninstall

all: install

install: $(TOOL)

clean:
	$(CLEANCURRY) -r

uninstall: clean
	rm -f $(TOOL)

# generate executable for Curry Browser:
Transform: $(DEPS)
	$(REPL) $(REPL_OPTS) :set -time :load Transform :save :quit
