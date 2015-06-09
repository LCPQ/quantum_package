#!/bin/bash 
# 
# Upgrades IRPF90 from the web.
# Wed Mar 25 11:41:04 CET 2015

if [[ -z ${QP_ROOT} ]]
then
  print "The QP_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
fi

cd -- ${QP_ROOT}
mv -f -- ${QP_ROOT}/irpf90 ${QP_ROOT}/irpf90.old

${QP_ROOT}/scripts/install/install_irpf90.sh

if [[ $? -eq 0 ]]
then
  rm -rf -- ${QP_ROOT}/irpf90.old
  echo "Successfully updated IRPF90"
else
  rm -rf -- ${QP_ROOT}/irpf90
  mv --  ${QP_ROOT}/irpf90.old ${QP_ROOT}/irpf90
  echo "Failed to update IRPF90"
fi
