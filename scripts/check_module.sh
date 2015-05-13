#!/bin/bash
#
# Checks to run in every Module directory before building

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QPACKAGE_ROOT}/scripts/qp_include.sh

check_current_dir_is_module

# Check if README.rst exists
if [[ ! -f README.rst ]] 
then
  ${QPACKAGE_ROOT}/scripts/module/create_rst_templates.sh
  error "
README.rst was not present, so I created a
default one for you.
You should document it before you compile, as
well as the ASSUMPTIONS.rst file.
"
else
  info "README.rst is present"
fi


# Check if ASSUMPTIONS.rst exists
if [[ ! -f ASSUMPTIONS.rst ]] 
then
  error "This is a Bug. At that point, the ASSUMPTIONS.rst) file should exist."
else
  info "ASSUMPTIONS.rst is present."
fi