#!/bin/bash -x

TARGET=zlib

function _install()
{
  mkdir ${TARGET} || exit 1
  cd ..
  QPACKAGE_ROOT=$PWD
  cd -
  cd ${BUILD}
  ./configure && make || exit 1
  make install prefix=$QPACKAGE_ROOT/install/${TARGET} || exit 1
}

source scripts/build.sh