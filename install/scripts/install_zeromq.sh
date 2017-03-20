#!/bin/bash -x

TARGET=zeromq

function _install()
{
  cd ..
  QP_ROOT=$PWD
  cd -
  set -e
  set -u
  ORIG=$(pwd)
  cd "${BUILD}"
  ./configure --prefix=$QP_ROOT --without-libsodium || exit 1
  make -j 8 || exit 1
  make install || exit 1
  cd ${ORIG}
  return 0
}

source scripts/build.sh
