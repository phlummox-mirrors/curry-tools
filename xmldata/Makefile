# Install tool to generate Curry programs for representing data in XML format

# Some modules required by the erd2curry implementation:
DEPS = Data2Xml.curry $(LIBDIR)/AbstractCurry/*.curry

# binaries
TOOL = $(BINDIR)/$(CURRYSYSTEM)-data2xml

.PHONY: all compile install clean uninstall runtest

all: install

compile: Data2Xml

install: compile
	rm -f $(TOOL) $(BINDIR)/data2xml
	cd $(BINDIR) && ln -s ../currytools/xmldata/Data2Xml $(notdir $(TOOL))

clean:
	$(CLEANCURRY)
	rm -f Data2Xml

uninstall: clean
	rm -f $(TOOL)

# generate executable for Data2Xml program:
Data2Xml: $(DEPS)
	$(REPL) $(REPL_OPTS) :load Data2Xml :save :q

# run the test suite
runtest:
	cd Examples && ./test.sh $(RUNTESTPARAMS)
