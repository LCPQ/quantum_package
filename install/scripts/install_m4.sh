#!/bin/bash -x

TARGET=m4

function _install()
{
  cd ..
  QP_ROOT=$PWD
  cd -
  cd ${BUILD}
  ./configure --prefix=$QP_ROOT && make || exit 1 
}

source scripts/build.sh
