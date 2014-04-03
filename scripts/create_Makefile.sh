#!/bin/bash
# 
# usage:
#  create_Makefile.sh MOs AOs Electrons
# Creates the Makefile . This command is supposed to be run in a 
# module directory.
# Thu Apr  3 01:44:41 CEST 2014

MODULE=$(basename $PWD)

if [[ $MODULE == "src" ]]
then
  echo "Error: This script should not be run in the src directory."
  exit 1
fi

cat << EOF > Makefile
default: all

# Define here all new external source files and objects.Don't forget to prefix the
# object files with IRPF90_temp/
SRC=
OBJ=

include \$(QPACKAGE_ROOT)/src/Makefile.common
EOF


