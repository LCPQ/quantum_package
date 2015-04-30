ifndef QPACKAGE_ROOT
  $(error "QPACKAGE_ROOT is not defined. Please source quantum_package.rc")
endif

include Makefile.config

ALL_MODULES=$(shell cat NEEDED_MODULES)
EZFIO_DIR=$(QPACKAGE_ROOT)/EZFIO
EZFIO=$(EZFIO_DIR)/lib/libezfio_irp.a

.PHONY: $(ALL_MODULES) ezfio


default: ezfio
	$(QPACKAGE_ROOT)/scripts/build_modules.sh $(ALL_MODULES)

veryclean:
	$(QPACKAGE_ROOT)/scripts/clean_modules.sh $(ALL_MODULES)
	# Define the dict [type in EZFIO.cfg] = ocaml type , f90 type
	# If you change the qptypes_generator.ml, you need to rm this
	# For simplicity add this to the veryclean rule
	rm $(QPACKAGE_ROOT)/scripts/ezfio_interface/fancy_type.p

$(ALL_MODULES): ezfio
	$(QPACKAGE_ROOT)/scripts/build_modules.sh $@ 

# Define the EZFIO rules
$(EZFIO): $(wildcard $(QPACKAGE_ROOT)/src/*/*.ezfio_config) $(wildcard $(QPACKAGE_ROOT)/src/*/EZFIO.cfg)
	$(QPACKAGE_ROOT)/scripts/ezfio_interface/prepare_ezfio.sh
	cd $(EZFIO_DIR);\
	export FC="$(FC)" ; export FCFLAGS="$(FCFLAGS)" ; export IRPF90="$(IRPF90)" ;\
	$(MAKE) ;\
	$(MAKE) Python

ezfio: $(EZFIO)

# Frequent typos
clena: clean
veryclena: veryclean

