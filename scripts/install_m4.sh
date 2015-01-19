#!/bin/bash
#
# Installs m4 for ocaml
# Thu Oct 23 22:02:08 CEST 2014

M4_URL="http://ftp.gnu.org/gnu/m4/m4-latest.tar.gz"
M4=$(which m4)

# Check the QPACKAGE_ROOT directory
if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit 1
fi

cd ${QPACKAGE_ROOT}

if [[ -z ${M4} ]] 
then 
    rm -f -- bin/m4
    ${QPACKAGE_ROOT}/scripts/fetch_from_web.py ${M4_URL} M4.tar.gz 
    tar -zxf M4.tar.gz && rm M4.tar.gz ||exit 1
    cd m4* || exit 1 
    ./configure && make || exit 1 
    ln -s ${PWD}/src/m4 ${QPACKAGE_ROOT}/bin 
else
    ln -s ${M4} ${QPACKAGE_ROOT}/bin/m4
fi

