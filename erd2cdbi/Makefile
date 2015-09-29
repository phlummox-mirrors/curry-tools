#****************************************************************************
# Transforms ERD-term into datatypes for CDBI.
# Creates information file for currypp SQL-Parser.
# Creates a corresponding sqlite database if invoked by option -db.
#
#****************************************************************************

# Tool binary
TOOL = $(BINDIR)/erd2cdbi

# Some modules required by the erd2cdbi tool:
DEPS = $(ROOT)/lib/Database/CDBI/*.curry ERD2CDBI.curry

.PHONY: all compile install clean uninstall

all: install

compile: erd2cdbi

install: compile
	rm -f $(TOOL)
	ln -s $(CURDIR)/erd2cdbi $(TOOL)

clean:  
	cleancurry -r
	rm -f erd2cdbi

uninstall: clean
	   rm -f $(TOOL)

erd2cdbi: $(DEPS)
	@echo Compiling ERD2CDBI-Tool
	$(REPL) $(REPL_OPTS) :load ERD2CDBI :save :quit
	mv ERD2CDBI $@
