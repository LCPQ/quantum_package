#!/bin/bash
#
# Installs EMSL_Basis_Set_Exchange_Local
# Mon Jan 12 12:57:19 CET 2015

BASE="EMSL_Basis_Set_Exchange_Local"
URL="https://github.com/LCPQ/${BASE}/archive/master.tar.gz"

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi

cd ${QPACKAGE_ROOT}

${QPACKAGE_ROOT}/scripts/fetch_from_web.py ${URL} ${QPACKAGE_ROOT}/${BASE}.tar.gz
tar -zxf ${BASE}.tar.gz && rm ${BASE}.tar.gz ||exit 1
rm -rf EMSL_Basis
mv ${BASE}-master EMSL_Basis


