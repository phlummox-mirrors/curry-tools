#****************************************************************************
# The ERD2CDBI tool transforms an ERD term into datatypes for CDBI.
# It also creates an information file for the currypp SQL-Parser.
# If the tool is invoked with the option -db, it creates a
# corresponding sqlite database.
#
#****************************************************************************

# Tool binary
TOOL = $(BINDIR)/erd2cdbi

# Some modules required by the erd2cdbi tool:
DEPS = $(ROOT)/lib/Database/CDBI/*.curry ERD2CDBI.curry

.PHONY: all compile install clean uninstall

all: install

compile: ERD2CDBI

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/ERD2CDBI $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f ERD2CDBI

uninstall: clean
	   rm -f $(TOOL)

ERD2CDBI: $(DEPS)
	@echo Compiling ERD2CDBI-Tool
	$(REPL) $(REPL_OPTS) :load ERD2CDBI :save :quit
