#!/bin/bash -x

TARGET=bats

function _install()
{
  cp -R ${BUILD} . || exit 1 
  cd ../bin
  ln -s ../install/${TARGET}/libexec/bats . || return 1
  cd -
}

source scripts/build.sh
