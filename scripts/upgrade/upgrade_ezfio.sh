#!/bin/bash
# 
# Upgrades the EZFIO library from the web.
# Tue Nov  4 00:53:13 CET 2014

if [[ -z ${QP_ROOT} ]]
then
  print "The QP_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
fi

cd -- ${QP_ROOT}
mv -- ${QP_ROOT}/EZFIO ${QP_ROOT}/EZFIO.old

${QP_ROOT}/scripts/install/install_ezfio.sh

if [[ $? -eq 0 ]]
then
  rm -rf -- ${QP_ROOT}/EZFIO.old
  echo "Successfully updated EZFIO"
else
  rm -rf -- ${QP_ROOT}/EZFIO
  mv --  ${QP_ROOT}/EZFIO.old ${QP_ROOT}/EZFIO
  echo "Failed to update EZFIO"
fi

