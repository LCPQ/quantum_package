#!/bin/bash

EZFIO=$1

if [[ -z ${EZFIO} ]]
then
  echo "Error in $0"
  exit 1
fi

if [[ ! -f ${EZFIO}/mo_basis/mo_label ]]
then
  LABEL='no_label'
else
  LABEL=$(head -1 ${EZFIO}/mo_basis/mo_label)
fi

DESTINATION="save/mo_basis/${LABEL}"

cd ${EZFIO}

if [[ ! -d save/mo_basis ]]
then
        mkdir -p save/mo_basis
fi

BACKUP=${DESTINATION}.old 
if [[ -d ${BACKUP} ]]
then
        rm -rf ${BACKUP}
fi

if [[ -d ${DESTINATION} ]]
then
        mv ${DESTINATION} ${BACKUP}
fi

cp -r mo_basis ${DESTINATION}

