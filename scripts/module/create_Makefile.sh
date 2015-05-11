#!/bin/bash
# 
# usage:
#  create_Makefile.sh MOs AOs Electrons
# Creates the Makefile. This command is supposed to be run in a 
# module directory.
# Thu Apr  3 01:44:41 CEST 2014

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QPACKAGE_ROOT}/scripts/qp_include.sh

check_current_dir_is_module

cat << EOF > Makefile
# Define here all new external source files and objects.Don't forget to prefix the
# object files with IRPF90_temp/
SRC=
OBJ=

include \$(QPACKAGE_ROOT)/src/Makefile.common
EOF


