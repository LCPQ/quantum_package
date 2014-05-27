default: all

SRC=
OBJ=

include $(QPACKAGE_ROOT)/src/Makefile.common

all: all_modules

all_clean:
	@for i in $(NEEDED_MODULES) ; do cd $$i ; make veryclean ; make clean_links ; cd .. ; done

all_modules: $(NEEDED_MODULES)

$(NEEDED_MODULES): FORCE
	@cd $@ ; unset NEEDED_MODULES INCLUDE_DIRS ; make 

