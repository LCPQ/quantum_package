#!/bin/bash
#
# Creates the initial NEEDED_MODULES file.
# This script is supposed to run in a module directory.
# Thu Apr  3 13:38:38 CEST 2014

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QPACKAGE_ROOT}/scripts/qp_include.sh

check_current_dir_is_module

OUTPUT=$(${QPACKAGE_ROOT}/scripts/check_dependencies.sh $@)
echo ${OUTPUT} > NEEDED_MODULES


