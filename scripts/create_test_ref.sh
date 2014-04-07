#!/bin/bash
# 
# usage:
#  create_test_ref.sh <test_executable>
# Creates the test reference file using all possible input directories of the
# data directory.
# Mon Apr  7 12:00:55 CEST 2014

TEST_EXE=$1
REF_FILE=${TEST_EXE}.ref


if [[ $(basename ${PWD}) != "tests" ]]
then
  echo "Error: This script should be run in the tests directory."
  exit 1
fi

if [[ -z ${TEST_EXE} ]]
then
  echo "usage: " $(basename $0) " <test_executable>"
  exit 1
fi

if [[ -z ${MAKE} ]]
then
  MAKE="make -j 4"
fi

echo "Unarchiving EZFIO input directories"
${MAKE} -C ${QPACKAGE_ROOT}/data/inputs all_ezfio > /dev/null

if [[ -f ${TEST_REF} ]]
then
  mv ${TEST_REF} ${TEST_REF}.save
fi

printf "data = {\n " > ${REF_FILE}
printf "Running tests...."
for dir in ${QPACKAGE_ROOT}/data/inputs/*.ezfio
do
  printf " '%s' : {\n " $(basename ${dir})
  ./${TEST_EXE} ${dir} | sed "s/\([^ ]*\) *\(:\) *\([^ ]*\)/'\1' : \3,/g"
  printf " },\n"
done >> ${REF_FILE}
printf "}\n" >> ${REF_FILE}
echo "Done."


