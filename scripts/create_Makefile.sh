#!/bin/bash
# 
# usage:
#  create_Makefile.sh MOs AOs Electrons
# Creates the Makefile with the dependencies on other modules given
# in the command line. This command is supposed to be run in a module
# directory and searches the dependencies in ../
# Thu Apr  3 01:44:41 CEST 2014

DEPS_LONG=""
for i in $@
do
  DEPS_LONG+=" $i "
  DEPS_LONG+=$(grep 'INCLUDE_DIRS' ${SCI_ROOT}/src/${i}/Makefile 2>/dev/null | cut -d '=' -f 2)
done

DEPS=($(
for d in $DEPS_LONG
do
  echo $d
done | sort | uniq
))

# Create the Makefile
cat << EOF > Makefile
default: all

# Define here all other modules on which the current module depends
INCLUDE_DIRS = ${DEPS[@]}

# Define here all new external source files and objects.Don't forget to prefix the
# object files with IRPF90_temp/
SRC=
OBJ=

include Makefile.depend
include \$(SCI_ROOT)/src/Makefile.config
include \$(SCI_ROOT)/src/Makefile.common
include irpf90.make

irpf90.make: \$(filter-out IRPF90_temp/%, \$(wildcard */*.irp.f)) \$(wildcard *.irp.f) \$(wildcard *.inc.f) Makefile \$(EZFIO)
	\$(IRPF90)

Makefile.depend: Makefile
	\$(SCI_ROOT)/scripts/create_Makefile_depend.sh
EOF


