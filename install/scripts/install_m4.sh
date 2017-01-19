#!/bin/bash -x

TARGET=m4

function _install()
{
  cd ..
  QP_ROOT=$PWD
  cd -
  cd ${BUILD}
  ./configure && make || exit 1 
  ln -sf ${PWD}/src/m4 ${QP_ROOT}/bin || exit 1 
}

source scripts/build.sh
