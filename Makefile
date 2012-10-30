# Generate various tools for Curry

# Required:
# - installed Curry System (PAKCS or KiCS2) specified by variable CURRYSYSTEM
# - root location of the Curry System specified by variable ROOT

.PHONY: all
all:
	@cd currydoc  ; $(MAKE)
	@cd genint    ; $(MAKE)

.PHONY: clean
clean: 
	cd analysis  ; $(ROOT)/bin/cleancurry
	cd currydoc  ; $(MAKE) clean
	cd genint    ; $(MAKE) clean
