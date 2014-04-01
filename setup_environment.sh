#!/bin/bash

SCI_ROOT=${PWD}

IRPF90=$(which irpf90) 

if [[ -z ${IRPF90} ]] ;
then
    make irpf90
    IRPF90=${SCI_ROOT}/irpf90/bin/irpf90
fi
make EZFIO

cat << EOF > sci.rc
export IRPF90=${IRPF90}
export SCI_ROOT=${SCI_ROOT}
export PATH+=:${SCI_ROOT}/scripts
export PATH+=:${SCI_ROOT}/bin
EOF
