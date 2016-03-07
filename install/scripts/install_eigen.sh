#!/bin/bash -x

TARGET=eigen

function _install()
{
  cp -R ${BUILD} . || exit 1 
}

source scripts/build.sh
