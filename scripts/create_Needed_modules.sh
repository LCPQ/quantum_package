#!/bin/bash
#
# Creates the initial NEEDED_MODULES file.
# This script is supposed to run in a module directory.
# Thu Apr  3 13:38:38 CEST 2014

MODULE=$(basename $PWD)

if [[ $MODULE == "src" ]]
then
  echo "Error: This script should not be run in the src directory."
  exit 1
fi

OUTPUT=$(${QPACKAGE_ROOT}/scripts/check_dependencies.sh $@)
echo ${OUTPUT} > NEEDED_MODULES


