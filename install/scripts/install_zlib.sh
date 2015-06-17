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
  make install prefix=$QP_ROOT/install/${TARGET} || exit 1
  ln -s -f $QP_ROOT/install/${TARGET}/lib/libz.so $QP_ROOT/lib || exit 1
  ln -s -f $QP_ROOT/install/${TARGET}/lib/libz.a $QP_ROOT/lib || exit 1
  ln -s -f $QP_ROOT/install/${TARGET}/include/zlib.h $QP_ROOT/lib || exit 1
  ln -s -f $QP_ROOT/install/${TARGET}/include/zconf.h $QP_ROOT/lib || exit 1
}

source scripts/build.sh
