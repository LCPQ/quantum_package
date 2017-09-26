#!/bin/bash -x

TARGET=gpi2
#GPI_OPTIONS=--with-infiniband 
GPI_OPTIONS=--with-ethernet

function _install()
{
  cd _build/gpi2
  ./install.sh -p $QP_ROOT $GPI_OPTIONS
  cp src/GASPI.f90 $QP_ROOT/plugins/GPI2/
  return 0
}

source scripts/build.sh
