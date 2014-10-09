all: inputs executables

.PHONY: inputs executables

inputs: 
	cd inputs ; $(MAKE) all_ezfio

executables: 
	$(MAKE) -C $(QPACKAGE_ROOT)/src executables && mv $(QPACKAGE_ROOT)/src/executables .
