default: all

SRC=
OBJ=

ALL_MODULES=$(shell cat NEEDED_MODULES)

.PHONY: executables all all_clean $(ALL_MODULES)

all: $(ALL_MODULES)

all_clean:
	@for i in $(ALL_MODULES) ; do cd $$i ; make veryclean ; make clean_links ; cd .. ; done

$(ALL_MODULES): FORCE
	cd $@ ; $(MAKE) 

executables: $(ALL_MODULES)
	rm -f executables ; \
	for EXE in $$(find $(QPACKAGE_ROOT)/src -perm /u+x -type f | grep -e "$(QPACKAGE_ROOT)/src/[^/]*/[^/]*$$" |sort ) ; \
	  do printf "%-30s %s\n" $$(basename $$EXE) $$EXE | sed "s|$(QPACKAGE_ROOT)|\$$QPACKAGE_ROOT|g" >> executables ;\
	done

# Dummy rule to enable to force recompilation
FORCE:

