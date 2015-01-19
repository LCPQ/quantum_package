#!/bin/bash
#
# Installs zlib for ocaml cryptokit
# Mon Jan 12 18:52:48 CET 2015

ZLIB="zlib-1.2.8"
ZLIB_URL="http://zlib.net/${ZLIB}.tar.gz"

# Check the QPACKAGE_ROOT directory
if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit 1
fi

cd ${QPACKAGE_ROOT}


cat > /tmp/main.c << EOF
int main () {}
EOF
gcc /tmp/main.c -lz
if [[ $? -eq 0 ]]
then
  rm /tmp/main.c
  exit 0
fi
rm /tmp/main.c

${QPACKAGE_ROOT}/scripts/fetch_from_web.py ${ZLIB_URL} ZLIB.tar.gz
tar -zxf ZLIB.tar.gz && rm ZLIB.tar.gz ||exit 1
cd ${ZLIB} || exit 1 
./configure && make
make install prefix=$QPACKAGE_ROOT
cd ${QPACKAGE_ROOT}
rm -rf -- ${ZLIB}

