# Generate various tools for Curry

# Required:
# - root location of the Curry System specified by variable ROOT

ROOT ?= $(error Please specify the variable ROOT)

export BINDIR     = $(ROOT)/bin
export LIBDIR     = $(ROOT)/lib
export METADIR    = $(LIBDIR)/meta
export CLEANCURRY = $(BINDIR)/cleancurry
export REPL       = $(BINDIR)/curry

# Directory names of all tools:
TOOLDIRS=$(notdir $(shell find . -mindepth 1 -maxdepth 1 -type d | sort))

make_TOOLDIRS=$(addprefix make_,$(TOOLDIRS))
compile_TOOLDIRS=$(addprefix compile_,$(TOOLDIRS))
install_TOOLDIRS=$(addprefix install_,$(TOOLDIRS))
clean_TOOLDIRS=$(addprefix clean_,$(TOOLDIRS))
uninstall_TOOLDIRS=$(addprefix uninstall_,$(TOOLDIRS))

.PHONY: all
all: $(make_TOOLDIRS)

$(make_TOOLDIRS):
	@cd $(patsubst make_%,%,$@) && $(MAKE)

.PHONY: compile
compile: $(compile_TOOLDIRS)

$(compile_TOOLDIRS):
	@cd $(patsubst compile_%,%,$@) && $(MAKE) compile

.PHONY: install
install: $(install_TOOLDIRS)

$(install_TOOLDIRS):
	@cd $(patsubst install_%,%,$@) && $(MAKE) install

.PHONY: clean
clean: $(clean_TOOLDIRS)

$(clean_TOOLDIRS):
	@cd $(patsubst clean_%,%,$@) && $(MAKE) clean

.PHONY: uninstall
uninstall: $(uninstall_TOOLDIRS)

$(uninstall_TOOLDIRS):
	@cd $(patsubst uninstall_%,%,$@) && $(MAKE) uninstall
