#!/bin/bash -x

TARGET=patch

function _install()
{  
  mkdir ${TARGET}
  cd ..
  QPACKAGE_ROOT=$PWD
  cd -
  cd ${BUILD}
  ./configure --prefix=${QPACKAGE_ROOT}/install/${TARGET} && make || exit 1
  make install || exit 1
  cd -
  cp ${TARGET}/bin/${TARGET} ${QPACKAGE_ROOT}/bin || exit 1
  rm -R -- ${TARGET} || exit 1
}

source scripts/build.sh