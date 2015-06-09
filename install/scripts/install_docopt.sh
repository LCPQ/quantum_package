#!/bin/bash -x

TARGET=docopt

function _install()
{
  cp -R ${BUILD} . || exit 1
}

source scripts/build.sh
