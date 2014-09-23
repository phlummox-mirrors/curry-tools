# Install tool to generate Curry programs for representing data in XML format
#
# Required:
# - installed Curry System (REPL)

# binaries
REPL       = $(ROOT)/bin/curry
CLEANCURRY = $(ROOT)/bin/cleancurry
INSTALLBIN = $(ROOT)/bin/data2xml

# Some modules required by the erd2curry implementation:
DEPS = Data2Xml.curry AbstractGoodies.curry

.PHONY: install
install: Data2Xml
	$(MAKE) uninstall
	ln -s $(CURDIR)/Data2Xml $(INSTALLBIN)

.PHONY: uninstall
uninstall:
	rm -f $(INSTALLBIN)

# generate executable for Data2Xml program:
Data2Xml: $(DEPS)
	$(REPL) $(REPL_OPTS) :load Data2Xml :save :q

.PHONY: clean
clean: uninstall
	$(CLEANCURRY)
	rm -f Data2Xml
