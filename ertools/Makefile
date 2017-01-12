#****************************************************************************
# The ERD2CDBI tool transforms an ERD term into datatypes for CDBI.
# It also creates an information file for the currypp SQL-Parser.
# If the tool is invoked with the option -db, it creates a
# corresponding sqlite database.
#
#****************************************************************************

# installation binaries
ERD2CURRY = $(BINDIR)/$(CURRYSYSTEM)-erd2curry
ERD2CDBI  = $(BINDIR)/$(CURRYSYSTEM)-erd2cdbi

# Some modules required by the erd2curry tool:
DEPSERD2CURRY = ERD2Curry.curry CodeGeneration.curry Transformation.curry \
                XML2ERD.curry ERD2Graph.curry \
                $(ROOT)/lib/Database/ERD*.curry $(LIBDIR)/AbstractCurry/*.curry

# Some modules required by the erd2cdbi tool:
DEPSERD2CDBI = ERD2CDBI.curry $(ROOT)/lib/Database/ERD*.curry \
               $(ROOT)/lib/Database/CDBI/*.curry 

.PHONY: all compile install clean uninstall

all: install

compile: ERD2Curry ERD2CDBI

install: compile
	rm -f $(ERD2CURRY) $(ERD2CDBI) $(BINDIR)/erd2curry $(BINDIR)/erd2cdbi
	cd $(BINDIR) && ln -s ../currytools/ertools/ERD2Curry $(notdir $(ERD2CURRY))
	cd $(BINDIR) && ln -s ../currytools/ertools/ERD2CDBI $(notdir $(ERD2CDBI))

clean:
	$(CLEANCURRY)
	rm -f erd2curry ERD2CDBI

uninstall: clean
	rm -f $(ERD2CURRY) $(ERD2CDBI)

ERD2Curry: $(DEPSERD2CURRY)
	$(REPL) $(REPL_OPTS) :load ERD2Curry :save :quit

ERD2CDBI: $(DEPSERD2CDBI)
	$(REPL) $(REPL_OPTS) :load ERD2CDBI :save :quit
