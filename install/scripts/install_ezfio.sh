#!/bin/bash -x

TARGET=ezfio

function _install()
{
  cd ..
  QP_ROOT=$PWD
  cd -
  rm -rf ${QP_ROOT}/EZFIO
  cd ${BUILD}/config || return 1
  cd -
  mv ${BUILD} ${QP_ROOT}/install/EZFIO || return 1
}

source scripts/build.sh
