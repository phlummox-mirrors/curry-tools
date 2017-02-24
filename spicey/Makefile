##############################################################################
# Makefile for Spicey Web Framework
##############################################################################

# binary
TOOL = $(BINDIR)/$(CURRYSYSTEM)-spiceup

# The directory containg the project generator
PROJECT_GEN_DIR=project_generator

# Some modules required by the Spicey implementation:
DEPS = $(PROJECT_GEN_DIR)/SpiceUp.curry scaffolding/*.curry \
       $(LIBDIR)/AbstractCurry/*.curry $(LIBDIR)/Database/ERD*.curry

.PHONY: all compile install clean uninstall runtest

all: install

compile: spiceup

install: compile
	rm -f $(TOOL) $(BINDIR)/spiceup
	cd $(BINDIR) && ln -s ../currytools/spicey/spiceup $(notdir $(TOOL))

clean:
	$(CLEANCURRY) -r
	rm -f spiceup

uninstall: clean
	rm -f $(TOOL)

# generate executable for Spicey generator:
spiceup: $(DEPS)
	CURRYPATH=scaffolding && export CURRYPATH && \
	cd $(PROJECT_GEN_DIR) && \
	  $(REPL) $(REPL_OPTS) :set path ../scaffolding :load SpiceUp :save :q
	mv $(PROJECT_GEN_DIR)/SpiceUp $@

# run the test suite
runtest:
	cd Examples && ./test.sh $(RUNTESTPARAMS)
