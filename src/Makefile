ifndef QPACKAGE_ROOT
  $(error "QPACKAGE_ROOT is not defined. Please source quantum_package.rc")
endif

include Makefile.config

ALL_MODULES=$(shell cat NEEDED_MODULES)
EZFIO_DIR=$(QPACKAGE_ROOT)/EZFIO
EZFIO=$(EZFIO_DIR)/lib/libezfio_irp.a

.PHONY: $(ALL_MODULES)


default: $(EZFIO)
	$(QPACKAGE_ROOT)/scripts/build_modules.sh $(ALL_MODULES)

veryclean:
	$(QPACKAGE_ROOT)/scripts/clean_modules.sh $(ALL_MODULES)

$(ALL_MODULES): $(EZFIO)
	$(QPACKAGE_ROOT)/scripts/build_modules.sh $@ 

executables: $(ALL_MODULES)
	rm -f executables ; \
	for EXE in $$(find $(QPACKAGE_ROOT)/src -perm /u+x -type f | grep -e "$(QPACKAGE_ROOT)/src/[^/]*/[^/]*$$" |sort ) ; \
	  do printf "%-30s %s\n" $$(basename $$EXE) $$EXE | sed "s|$(QPACKAGE_ROOT)|\$$QPACKAGE_ROOT|g" >> executables ;\
	done

# Define the EZFIO rules
$(EZFIO): $(wildcard $(QPACKAGE_ROOT)/src/*.ezfio_config) $(wildcard $(QPACKAGE_ROOT)/src/*/EZFIO.cfg)
	$(QPACKAGE_ROOT)/scripts/prepare_ezfio.sh
	cd $(EZFIO_DIR);\
	export FC="$(FC)" ; export FCFLAGS="$(FCFLAGS)" ; export IRPF90="$(IRPF90)" ;\
	$(MAKE) ;\
	$(MAKE) Python

# Frequent typos
clena: clean
veryclena: veryclean

