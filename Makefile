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

resultsFile: 
	$(info $(BLUE)===== Installing resultsFile ===== $(BLACK))
	@sleep 1
	$(QPACKAGE_ROOT)/scripts/install_resultsFile.sh

EZFIO: bin/irpf90
	$(info $(BLUE)===== Installing EZFIO ===== $(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_ezfio.sh | tee install_ezfio.log

EMSL_Basis: 
	$(info $(BLUE)===== Installing EMSL_Basis_Set_Exchange_Local ===== $(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_emsl.sh | tee install_emsl.log

zlib: 
	$(info $(BLUE)===== Installing Zlib ===== $(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_zlib.sh | tee install_zlib.log


bin/irpf90:
	$(info $(BLUE)===== Installing IRPF90 ===== $(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_irpf90.sh | tee install_irpf90.log

doc:
	$(MAKE) -C doc

src: irpf90 EZFIO ocaml
	@export QPACKAGE_ROOT=$$PWD ; \
	$(MAKE) -C src

bin/curl:
	$(info $(BLUE)===== Installing curl =====$(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_curl.sh | tee install_curl.log

bin/m4:
	$(info $(BLUE)===== Installing m4 =====$(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_m4.sh | tee install_m4.log


ocaml: curl m4 emsl zlib
	- rm -f -- ocaml/Qptypes.ml
	$(MAKE) ocaml/Qptypes.ml

ocaml/Qptypes.ml: 
	$(info $(BLUE)===== Installing ocaml =====$(BLACK))
	@sleep 1
	QPACKAGE_ROOT=$$PWD ./scripts/install_ocaml.sh | tee install_ocaml.log

