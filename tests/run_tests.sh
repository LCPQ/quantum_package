#!/bin/bash -e

LIST="
convert.bats
hf.bats
fci.bats
cassd.bats
mrcepa0.bats
pseudo.bats
"
#foboci.bats


export QP_PREFIX="timeout -s 9 600"
#export QP_TASK_DEBUG=1

rm -rf work output


for BATS_FILE in $LIST
do
  echo
  echo "-~-~-~-~-~-~"
  echo
  echo "Running tests for ${BATS_FILE%.bats}"
  echo
  BATS_FILE=bats/$BATS_FILE
  if [[ "$1" == "-v" ]]
  then
    echo "Verbose mode"
    ./bats_to_sh.py $BATS_FILE | bash 
  else
    bats $BATS_FILE
  fi
done

