default: all

SRC=
OBJ=

include $(QPACKAGE_ROOT)/src/Makefile.common

.PHONY: executables

all: all_modules

all_clean:
	@for i in $(NEEDED_MODULES) ; do cd $$i ; make veryclean ; make clean_links ; cd .. ; done

all_modules: $(NEEDED_MODULES)

$(NEEDED_MODULES): FORCE
	@cd $@ ; unset NEEDED_MODULES INCLUDE_DIRS ; make 

executables:
	rm -f executables ; \
	for EXE in $$(find $(QPACKAGE_ROOT)/src -type f -executable | grep -e "$(QPACKAGE_ROOT)/src/[^/]*/[^/]*$$" ) ; \
	  do printf "%-30s %s\n" $$(basename $$EXE) $$EXE | sed "s|$(QPACKAGE_ROOT)|\$$QPACKAGE_ROOT|g" >> executables ;\
	done

