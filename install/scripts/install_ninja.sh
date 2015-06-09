#!/bin/bash -x

TARGET=ninja

function _install()
{
  cd ${BUILD} || return 1
  ./configure.py --bootstrap || return 1
  cd -
  mv ${BUILD}/ninja ../bin/ || return 1
  return 0 
}

source scripts/build.sh