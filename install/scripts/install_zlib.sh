#!/bin/bash -x

TARGET=zlib

function _install()
{
  rm -rf -- ${TARGET}
  mkdir ${TARGET} || exit 1
  cd ..
  QP_ROOT=$PWD
  cd -
  cd ${BUILD}
  ./configure && make || exit 1
  ./configure --prefix=$QP_ROOT && make || exit 1 
  make install || exit 1
}

source scripts/build.sh
