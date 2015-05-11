#!/bin/bash
#
# Installs curl for ocaml
# Mon Jan 12 18:52:48 CET 2015

CURL="curl-7.30.0.ermine"
CURL_URL="http://qmcchem.ups-tlse.fr/files/scemama/${CURL}.tar.bz2"

if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit -1
fi

cd ${QPACKAGE_ROOT}

curl -kL "https://github.com/LCPQ/quantum_package" &> /dev/null
if [[ $? -eq 0 ]]
then
  exit 0
fi

rm -f -- ${QPACKAGE_ROOT}/bin/curl
${QPACKAGE_ROOT}/scripts/fetch_from_web.py ${CURL_URL} CURL.tar.bz2
tar -jxf CURL.tar.bz2 && rm CURL.tar.bz2 ||exit 1
cd ${CURL} || exit 1 
mv curl.ermine ${QPACKAGE_ROOT}/bin/curl
cd ${QPACKAGE_ROOT}
rm -rf -- ${CURL}

