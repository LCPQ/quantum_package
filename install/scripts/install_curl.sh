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

source scripts/build.sh
