#!/bin/bash -x

TARGET=f77zmq

function _install()
{
  export C_INCLUDE_PATH="${C_INCLUDE_PATH}":"${QP_ROOT}"/lib
  set -e
  set -u
  export ZMQ_H="${QP_ROOT}"/lib/zmq.h
  cd "${BUILD}"
  make -j 8                        || exit 1
  mv libf77zmq.a "${QP_ROOT}"/lib  || exit 1
  mv libf77zmq.so "${QP_ROOT}"/lib || exit 1
  cd -
  return 0
}

source scripts/build.sh
