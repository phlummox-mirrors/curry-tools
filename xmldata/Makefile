# Install tool to generate Curry programs for representing data in XML format
#
# Required:
# - installed Curry System (REPL)

# Some modules required by the erd2curry implementation:
DEPS = Data2Xml.curry AbstractGoodies.curry

# binaries
TOOL = $(ROOT)/bin/data2xml

.PHONY: all compile install clean uninstall

all: compile install

compile: Data2Xml

install: Data2Xml uninstall
	ln -s $(CURDIR)/Data2Xml $(TOOL)

clean: uninstall
	$(CLEANCURRY)
	rm -f Data2Xml

uninstall:
	rm -f $(TOOL)

# generate executable for Data2Xml program:
Data2Xml: $(DEPS)
	$(REPL) $(REPL_OPTS) :load Data2Xml :save :q
