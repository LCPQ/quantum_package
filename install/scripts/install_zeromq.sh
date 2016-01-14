#!/bin/bash -x

TARGET=zeromq

function _install()
{
  cd ..
  QP_ROOT=$PWD
  cd -
  export C_INCLUDE_PATH="${C_INCLUDE_PATH}":./
  set -e
  set -u
  ORIG=$(pwd)
  cd "${BUILD}"
  ./configure --without-libsodium || exit 1
  make -j 8 || exit 1
  rm -f -- "${QP_ROOT}"/lib/libzmq.a "${QP_ROOT}"/lib/libzmq.so "${QP_ROOT}"/lib/libzmq.so.?
# cp .libs/libzmq.a  "${QP_ROOT}"/lib 
# cp .libs/libzmq.so "${QP_ROOT}"/lib/libzmq.so.5
  cp src/.libs/libzmq.a  "${QP_ROOT}"/lib 
  cp src/.libs/libzmq.so "${QP_ROOT}"/lib/libzmq.so.4
  cp include/{zmq.h,zmq_utils.h} "${QP_ROOT}"/lib
  cd "${QP_ROOT}"/lib
# ln -s libzmq.so.5 libzmq.so
  ln -s libzmq.so.4 libzmq.so
  cd ${ORIG}
  return 0
}

source scripts/build.sh
