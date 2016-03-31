#!/bin/bash

export QP_PREFIX="timeout -s 9 300"
export QP_TASK_DEBUG=1

BATS_FILE=bats/qp.bats 

rm -rf work output

if [[ "$1" == "-v" ]]
then
  echo "Verbose mode"
  ./bats_to_sh.py $BATS_FILE | bash
else
  bats $BATS_FILE
fi


