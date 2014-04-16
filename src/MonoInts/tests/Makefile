IRPF90+= -I tests

REF_FILES=$(subst %.irp.f, %.ref, $(wildcard *.irp.f))

.PHONY: clean executables serial_tests parallel_tests

all: clean executables serial_tests parallel_tests

parallel_tests: $(REF_FILES)
	@echo ; echo "   ----  Running parallel tests ----" ; echo
	@OMP_NUM_THREADS=10 ${QPACKAGE_ROOT}/scripts/run_tests.py

serial_tests: $(REF_FILES)
	@echo ; echo "   ----  Running serial tests ----" ; echo
	@OMP_NUM_THREADS=1 ${QPACKAGE_ROOT}/scripts/run_tests.py

executables: $(wildcard *.irp.f) veryclean
	$(MAKE) -C ..

%.ref: $(wildcard $(QPACKAGE_ROOT)/data/inputs/*.md5) executables
	$(QPACKAGE_ROOT)/scripts/create_test_ref.sh $*

clean:
	$(MAKE) -C .. clean

veryclean:
	$(MAKE) -C .. veryclean


