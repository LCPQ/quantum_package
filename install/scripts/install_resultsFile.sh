#!/bin/bash -x

TARGET=resultsFile

function _install()
{
  cp -R ${BUILD} . || exit 1
}

source scripts/build.sh

