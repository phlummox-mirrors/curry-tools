# Generate various tools for Curry

# Required:
# - root location of the Curry System specified by variable ROOT

ROOT ?= $(error Please specify the variable ROOT)

export BINDIR     = $(ROOT)/bin
export LIBDIR     = $(ROOT)/lib
export CLEANCURRY = $(BINDIR)/cleancurry
export REPL       = $(BINDIR)/curry

# Directory names of all tools:
ALLTOOLDIRS = $(filter-out $(EXCLUDES), $(sort $(notdir $(shell find . -mindepth 1 -maxdepth 1 -type d))))
EXCLUDES = .git
# Directory names of all tools having a Makefile:
TOOLDIRS = $(foreach d, $(ALLTOOLDIRS), $(shell test -f $(d)/Makefile && echo $(d)))

compile_TOOLDIRS=$(addprefix compile_,$(TOOLDIRS))
install_TOOLDIRS=$(addprefix install_,$(TOOLDIRS))
clean_TOOLDIRS=$(addprefix clean_,$(TOOLDIRS))
uninstall_TOOLDIRS=$(addprefix uninstall_,$(TOOLDIRS))

.PHONY: all
all: $(TOOLDIRS)

.PHONY: force

$(TOOLDIRS): force
	@cd $@ && $(MAKE)

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

# run the test suites to check the tools
.PHONY: runtest
runtest:
	cd optimize/binding_optimization/Examples && ./test.sh
	cd currypp  && $(MAKE) runtest
	cd runcurry/Examples && ./test.sh
	cd currycheck/Examples && ./test.sh
	cd currycheck/Examples/WithVerification && ./test.sh
	cd spicey/Examples && ./test.sh
	cd xmldata/Examples && ./test.sh
