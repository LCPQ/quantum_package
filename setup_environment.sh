#!/bin/bash
#
# Setup script. Downloads IRPF90 and EZFIO if they are not already present in the current
# installation.
# Mon Apr  7 15:41:19 CEST 2014

QPACKAGE_ROOT=${PWD}

IRPF90=$(which irpf90) 

if [[ -z ${IRPF90} ]] ;
then
    make irpf90
    IRPF90=${QPACKAGE_ROOT}/irpf90/bin/irpf90
    if [[ ! -x ${QPACKAGE_ROOT}/irpf90/bin/irpf90 ]]
    then
      echo "Error in IRPF90 installation"
      exit 1
    fi
fi

cat << EOF > quantum_package.rc
export IRPF90=${IRPF90}
export QPACKAGE_ROOT=${QPACKAGE_ROOT}
export PYTHONPATH+=:\${QPACKAGE_ROOT}/scripts
export PATH+=:\${QPACKAGE_ROOT}/scripts
export PATH+=:\${QPACKAGE_ROOT}/bin
export QPACKAGE_CACHE_URL="http://qmcchem.ups-tlse.fr/files/scemama/quantum_package/cache"
EOF

source quantum_package.rc
if [[ ! -d ${QPACKAGE_ROOT}/EZFIO ]]
then
make EZFIO
fi

if [[ ! -d ${QPACKAGE_ROOT}/EZFIO ]]
then
  echo "Error in IRPF90 installation"
  exit 1
fi
