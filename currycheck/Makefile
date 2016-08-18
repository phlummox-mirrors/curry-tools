# Makefile for generating the currycheck tool

TOOL=$(BINDIR)/currycheck

REWRITINGDIR  = $(CURDIR)/../verification

# currycheck input path and dependencies:
CCPATH = $(REWRITINGDIR)
DEPS = *.curry $(LIBDIR)/Test/EasyCheck.curry $(LIBDIR)/AbstractCurry/*.curry

.PHONY: all compile install clean uninstall

all: install

compile: CurryCheck

install: CurryCheck
	rm -f $(TOOL)
	ln -s $(CURDIR)/CurryCheck $(TOOL)

clean:
	$(CLEANCURRY)
	rm -f CurryCheck

uninstall: clean
	rm -f $(TOOL)

CurryCheck: $(DEPS)
	$(REPL) $(REPL_OPTS) :set path $(CCPATH) :l CurryCheck :save :q
