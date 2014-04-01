default: all
INCLUDE_DIRS = Ezfio_files Nuclei Utils AOs Electrons # MonoInts BiInts MOs Output Bitmask

include Makefile.common
include Makefile.config

LIB+=$(MKL) 

SRC=#BiInts/map_module.f90 Bitmask/bitmasks_module.f90
OBJ=#IRPF90_temp/BiInts/map_module.o IRPF90_temp/Bitmask/bitmasks_module.o
PYTHON_SCRIPTS=

include irpf90.make
all:$(ALL) $(PYTHON_SCRIPTS) 

irpf90.make: $(filter-out IRPF90_temp/%, $(wildcard */*.irp.f)) $(wildcard *.irp.f) $(wildcard *.inc.f) Makefile $(EZFIO)
	$(IRPF90)

all_clean:
	for i in */ ; do $(MAKE) -C $$i veryclean clean_links ; done

