# Makefile for generating the currycheck tool

TOOL=$(BINDIR)/$(CURRYSYSTEM)-check

# currycheck (main) dependencies:
DEPS = *.curry $(LIBDIR)/Test/EasyCheck.curry $(LIBDIR)/AbstractCurry/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: CurryCheck

install: CurryCheck
	rm -f $(TOOL) $(BINDIR)/currycheck
	ln -s $(CURDIR)/CurryCheck $(TOOL)
	ln -s $(TOOL) $(BINDIR)/currycheck # for backward compatibility

clean:
	$(CLEANCURRY)
	rm -f CurryCheck

uninstall: clean
	rm -f $(TOOL)

CurryCheck: $(DEPS)
	$(REPL) $(REPL_OPTS) :l CurryCheck :save :q
