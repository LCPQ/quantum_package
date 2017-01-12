#!/bin/bash -x

TARGET=f77zmq

function _install()
{
  cd ..
  QP_ROOT=$PWD
  cd -
  set -e
  set -u
  export ZMQ_H="${QP_ROOT}"/include/zmq.h
  cd "${BUILD}"
  make -j 8                        || exit 1
  mv libf77zmq.a "${QP_ROOT}"/lib  || exit 1
  mv libf77zmq.so "${QP_ROOT}"/lib || exit 1
  cp f77_zmq.h "${QP_ROOT}"/src/ZMQ/
  cd -
  return 0
}

source scripts/build.sh
