#!/bin/bash -x

TARGET=irpf90
function _install()
{
  cd ..
  QPACKAGE_ROOT=$PWD
  cd -

  make -C ${BUILD} || return 1
  rm -rf -- ./irpf90 
  mv ${BUILD} .  || return 1
  [[ -x ./irpf90/bin/irpf90 ]] || return 1
  [[ -x ./irpf90/bin/irpman ]] || return 1
  rm -rf -- ../bin/irpf90 ../bin/irpman
  echo 'exec ${QMCCHEM_PATH}/install/irpf90/bin/irpf90 $@' > ../bin/irpf90 || return 1
  echo 'exec ${QMCCHEM_PATH}/install/irpf90/bin/irpman $@' > ../bin/irpman || return 1
  chmod +x ../bin/irpf90 ../bin/irpman || return 1
  return 0
}

source scripts/build.sh


