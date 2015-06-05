#!/bin/bash -x

TARGET=ezfio

function _install()
{
  cd ..
  QPACKAGE_ROOT=$PWD
  cd -
  rm -rf ${QPACKAGE_ROOT}/EZFIO
  cd ${BUILD}/config || return 1
  cd -
  mv ${BUILD} ${QPACKAGE_ROOT}/install/EZFIO || return 1
}

source scripts/build.sh
