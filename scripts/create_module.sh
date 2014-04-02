#!/bin/bash
# 
# usage:
#  create_module.sh MOs AOs Electrons
# Prepares all the files for the creation of a new module.
# The first argument is the name of the module
# All remaining aruments are dependencies.
# Thu Apr  3 01:44:58 CEST 2014

MODULE=$1
shift
DEPS=$@

# Check if module already exists
if [ -d ${MODULE} ]
then
  echo "Error: Module $MODULE already exists"
  exit 1
fi

# Create module directory and go into it
mkdir ${MODULE}
cd ${MODULE}

# Create the Makefile
${SCI_ROOT}/create_Makefile.sh


