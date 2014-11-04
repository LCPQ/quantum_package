#!/bin/bash
# 
# Upgrades IRPF90 from the web.
# Tue Nov  4 00:53:13 CET 2014

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
fi

cd -- ${QPACKAGE_ROOT}
mv -- ${QPACKAGE_ROOT}/irpf90 ${QPACKAGE_ROOT}/irpf90.old

make irpf90 

if [[ $? -eq 0 ]]
then
  rm -rf -- ${QPACKAGE_ROOT}/irpf90.old
  echo "Successfully updated IRPF90"
else
  rm -rf -- ${QPACKAGE_ROOT}/irpf90
  mv --  ${QPACKAGE_ROOT}/irpf90.old ${QPACKAGE_ROOT}/irpf90
  echo "Failed to update IRPF90"
fi
