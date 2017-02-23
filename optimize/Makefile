# Generate optimization tools for Curry

# Required:
# - root location of the Curry System specified by variable ROOT

.PHONY: all compile install clean uninstall runtest

all:
	@cd binding_optimization && $(MAKE) all

compile:
	@cd binding_optimization && $(MAKE) compile

install:
	@cd binding_optimization && $(MAKE) install

clean:
	@cd binding_optimization && $(MAKE) clean

uninstall:
	@cd binding_optimization && $(MAKE) uninstall

# run the test suite
runtest:
	cd binding_optimization/Examples && ./test.sh $(RUNTESTPARAMS)
