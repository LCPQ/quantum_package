BLUE=[34m
BLACK=(B[m


.PHONY: doc src ocaml build binary

default: 
	@echo   -----------------------------------------------
	@echo To set up the environment, run
	@echo ./setup_environment.sh
	@echo
	@echo To compile everything, run
	@echo make build
	@echo
	@echo To compile a binary distribution for export, run
	@echo make binary
	@echo   -----------------------------------------------

ifndef QPACKAGE_ROOT
build:
	@echo   -------------------- Error --------------------
	@echo   QPACKAGE_ROOT undefined.
	@echo   Run
	@echo     ./setup_environment.sh
	@echo   or
	@echo     source quantum_package.rc
	@echo   -----------------------------------------------
else
build: 
	$(MAKE) -C src
	$(MAKE) -C ocaml
endif

binary: 
	$(QPACKAGE_ROOT)/scripts/make_binary.sh

doc:
	$(MAKE) -C doc

src:
	$(MAKE) -C src

ocaml:
	$(MAKE) ocaml/Qptypes.ml

veryclean:
	rm -f EZFIO
	$(MAKE) EZFIO
	$(MAKE) -C src veryclean
