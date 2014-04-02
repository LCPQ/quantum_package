default: all

# Define here all other modules on which the current module depends
INCLUDE_DIRS = AOs Ezfio_files Nuclei Utils

# Define here all new external source files and objects.Don't forget to prefix the
# object files with IRPF90_temp/
SRC=
OBJ=

include Makefile.depend
include $(QPACKAGE_ROOT)/src/Makefile.config
include $(QPACKAGE_ROOT)/src/Makefile.common
include irpf90.make

irpf90.make: $(filter-out IRPF90_temp/%, $(wildcard */*.irp.f)) $(wildcard *.irp.f) $(wildcard *.inc.f) Makefile $(EZFIO)
	$(IRPF90)

Makefile.depend: Makefile
	$(QPACKAGE_ROOT)/scripts/create_Makefile_depend.sh
