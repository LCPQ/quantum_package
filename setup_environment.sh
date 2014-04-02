#!/bin/bash

QPACKAGE_ROOT=${PWD}

IRPF90=$(which irpf90) 

if [[ -z ${IRPF90} ]] ;
then
    make irpf90
    IRPF90=${QPACKAGE_ROOT}/irpf90/bin/irpf90
fi
make EZFIO

cat << EOF > quantum_package.rc
export IRPF90=${IRPF90}
export QPACKAGE_ROOT=${QPACKAGE_ROOT}
export PATH+=:${QPACKAGE_ROOT}/scripts
export PATH+=:${QPACKAGE_ROOT}/bin
EOF
