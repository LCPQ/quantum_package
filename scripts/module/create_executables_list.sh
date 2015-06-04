#!/bin/bash 
#
# Thu Mar 26 01:27:14 CET 2015

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi

cd ${QPACKAGE_ROOT}/data
rm -f executables
EXES=$(find ${QPACKAGE_ROOT}/src -perm /u+x -type f | grep -e "${QPACKAGE_ROOT}/src/[^/]*/[^/]*$" |sort ) 

for EXE in $EXES
do
   printf "%-30s %s\n" $(basename $EXE) $EXE | sed "s|${QPACKAGE_ROOT}|\$QPACKAGE_ROOT|g" >> executables 
done





