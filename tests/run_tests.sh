#!/bin/bash

LIST="

convert.bats
hf.bats
foboci.bats
pseudo.bats
fci.bats
cassd.bats
mrcepa0.bats

"

export QP_PREFIX="timeout -s 9 300"
export QP_TASK_DEBUG=1
rm -rf work output

if [[ "$1" == "-v" ]]
then
  for BATS_FILE in $LIST
  do
    echo
    echo "-~-~-~-~-~-~"
    echo
    echo "Running tests for ${BATS_FILE%.bats}"
    echo
    BATS_FILE=bats/$BATS_FILE
    echo "Verbose mode"
    ./bats_to_sh.py $BATS_FILE | bash
   done
else
   cd bats
   bats .
fi

