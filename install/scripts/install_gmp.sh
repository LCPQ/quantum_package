#!/bin/bash -x

TARGET=gmp

function _install()
{
  rm -rf -- ${TARGET}
  mkdir ${TARGET} || exit 1
  cd ..
  QP_ROOT=$PWD
  cd -
  cd ${BUILD}
  ./configure --prefix=$QP_ROOT && make -j 8 || exit 1
  make install || exit 1
}

source scripts/build.sh
