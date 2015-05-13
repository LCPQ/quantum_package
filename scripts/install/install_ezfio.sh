#!/bin/bash
#
# Installs EZFIO
# Mon Jan 12 16:06:44 CET 2015

BASE="ezfio"
URL="https://github.com/LCPQ/${BASE}/archive/master.tar.gz"

if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit -1
fi

cd ${QPACKAGE_ROOT}

rm -rf -- EZFIO
${QPACKAGE_ROOT}/scripts/install/fetch_from_web.py ${URL} ${QPACKAGE_ROOT}/${BASE}.tar.gz
tar -zxf ${BASE}.tar.gz && rm ${BASE}.tar.gz ||exit 1
mv EZFIO-master EZFIO



