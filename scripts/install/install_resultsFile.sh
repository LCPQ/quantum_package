#!/bin/bash
#
# Installs the resultsFile Python library
# Mon Jan 19 15:08:18 CET 2015

URL="https://github.com/LCPQ/resultsFile/archive/master.tar.gz"

# Check the QPACKAGE_ROOT directory
if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit 1
fi

cd ${QPACKAGE_ROOT}


rm -rf resultsFile-master
${QPACKAGE_ROOT}/scripts/fetch_from_web.py ${URL} ${QPACKAGE_ROOT}/resultsFile.tar.gz
tar -zxf resultsFile.tar.gz && rm resultsFile.tar.gz ||exit 1
mv resultsFile-master resultsFile


