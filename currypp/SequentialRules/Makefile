# Makefile for sequential rules preprocessor

# source modules of the seqeuential rules preprocessor:
DEPS = *.curry

.PHONY: all compile install clean uninstall

all: install

install: Main

clean:
	$(CLEANCURRY) -r
	rm -f Main

uninstall: clean
	rm -f $(TOOL)

# generate executable for Curry Browser:
Main: $(DEPS)
	$(REPL) $(REPL_OPTS) :set -time :load Main :save :quit
