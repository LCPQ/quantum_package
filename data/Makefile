all: inputs executables

.PHONY: inputs executables

inputs: 
	cd inputs ; $(MAKE) all_ezfio

executables: 
	cd  $(QPACKAGE_ROOT)/src ; $(MAKE) executables 
	mv $(QPACKAGE_ROOT)/src/executables .
