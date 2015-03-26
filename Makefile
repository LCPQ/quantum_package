BLUE=[34m
BLACK=(B[m


.PHONY: doc src curl m4 ocaml irpf90 emsl build binary

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
build: EZFIO curl m4 irpf90 emsl 
	$(MAKE) -C src
	$(MAKE) -C ocaml
endif

curl: bin/curl
m4: bin/m4
irpf90: bin/irpf90
emsl: EMSL_Basis

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
