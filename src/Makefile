default: all
NEEDED_MODULES=$(shell cat NEEDED_MODULES)

SRC=
OBJ=

include Makefile.depend
include $(QPACKAGE_ROOT)/src/Makefile.config
include $(QPACKAGE_ROOT)/src/Makefile.common
include irpf90.make

all:$(ALL)

irpf90.make: $(filter-out IRPF90_temp/%, $(wildcard */*.irp.f)) $(wildcard *.irp.f) $(wildcard *.inc.f) Makefile $(EZFIO) NEEDED_MODULES
	$(IRPF90)

Makefile.depend: Makefile NEEDED_MODULES
	$(QPACKAGE_ROOT)/scripts/create_Makefile_depend.sh

all_clean:
	@for i in $(NEEDED_MODULES) ; do cd $$i ; make veryclean ; make clean_links ; cd .. ; done

all_modules: $(NEEDED_MODULES)

FORCE:

$(NEEDED_MODULES): FORCE
	@cd $@ ; unset NEEDED_MODULES INCLUDE_DIRS ; make 

