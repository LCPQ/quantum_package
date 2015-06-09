#!/bin/bash -x

TARGET=emsl

function _install()
{
  cp -R ${BUILD} . || exit 1
}

source scripts/build.sh
