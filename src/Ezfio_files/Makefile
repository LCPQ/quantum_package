default: all

include ../Makefile.config
include ../Makefile.common
include irpf90.make

irpf90.make: $(filter-out IRPF90_temp/%, $(wildcard */*.irp.f)) $(wildcard *.irp.f) $(wildcard *.inc.f) Makefile $(EZFIO)
	$(IRPF90)
