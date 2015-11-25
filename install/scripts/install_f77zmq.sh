#!/bin/bash -x

TARGET=f77zmq

function _install()
{
  set -e
  set -u
  export C_INCLUDE_PATH="${C_INCLUDE_PATH}":"${QP_ROOT}"/lib
  export ZMQ_H=$PWD/zmq.h
  cd "${BUILD}"
  make -j 8 
  mv libf77zmq.a "${QP_ROOT}"/lib
  mv libf77zmq.so "${QP_ROOT}"/lib
  cd -
  return 0
}

source scripts/build.sh
