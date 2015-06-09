#!/bin/bash -x
TARGET=curl

function _install()
{
  cd ..
  QP_ROOT=$PWD
  cd -
  cd ${BUILD} || return 1
  mv curl.ermine ${QP_ROOT}/bin/curl || return 1
}

BUILD=_build/${TARGET}
rm -rf -- ${BUILD}
mkdir ${BUILD} || exit 1
tar -xvjf Downloads/${TARGET}.tar.bz2 --strip-components=1 --directory=${BUILD} || exit 1
_install || exit 1
rm -rf -- ${BUILD} _build/${TARGET}.log
exit 0