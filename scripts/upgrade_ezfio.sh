#!/bin/bash
# 
# Upgrades the EZFIO library from the web.
# Tue Nov  4 00:53:13 CET 2014

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
fi

cd -- ${QPACKAGE_ROOT}
mv -- ${QPACKAGE_ROOT}/EZFIO ${QPACKAGE_ROOT}/EZFIO.old

make EZFIO

if [[ $? -eq 0 ]]
then
  rm -rf -- ${QPACKAGE_ROOT}/EZFIO.old
  echo "Successfully updated EZFIO"
else
  rm -rf -- ${QPACKAGE_ROOT}/EZFIO
  mv --  ${QPACKAGE_ROOT}/EZFIO.old ${QPACKAGE_ROOT}/EZFIO
  echo "Failed to update EZFIO"
fi

