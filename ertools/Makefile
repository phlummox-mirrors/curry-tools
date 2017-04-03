#****************************************************************************
# The ERD2Curry tool transforms an ERD term into datatypes for CDBI.
# It also creates an information file for the currypp SQL-Parser.
# If the tool is invoked with the option --db, it creates a
# corresponding sqlite database.
#
#****************************************************************************

# The tool name of the application:
TOOL = $(BINDIR)/$(CURRYSYSTEM)-erd2curry

# The load path of the application:
LOADPATH = src

# Some modules required by the erd2curry tool:
DEP = src/*.curry \
      $(ROOT)/lib/Database/ERD*.curry $(ROOT)/lib/Database/CDBI/*.curry \
      $(LIBDIR)/AbstractCurry/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: ERD2Curry

install: compile
	rm -f $(TOOL)
	cd $(BINDIR) && ln -s ../currytools/ertools/ERD2Curry $(notdir $(TOOL))

clean:
	cd src && $(CLEANCURRY)

uninstall: clean
	rm -f ERD2Curry $(TOOL)

ERD2Curry: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(LOADPATH) :load ERD2Curry :save :quit
