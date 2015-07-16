#!/bin/bash -x

TARGET=p_graphviz

function _install()
{
  cp -R ${BUILD} . || exit 1
}

source scripts/build.sh
