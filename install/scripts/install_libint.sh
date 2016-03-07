#!/bin/bash -x

TARGET=libint

function _install()
{

  cd ..
  QP_ROOT=$PWD
  cd -

  cp -R ${BUILD} . || exit 1 

  cd ${TARGET}
  export CXX="g++"
  export CXXFLAGS=" -O3 -std=c++0x"
  ./configure --with-cxx-optflags
  make -j 8
  cd -
  g++ -O2 -std=c++0x -DHAVE_CONFIG_H  -I${PWD}/${TARGET}/include -I${QP_ROOT}/install/eigen -DPREP_LIBINT2_SKIP_BOOST -L${PWD}/${TARGET}/lib -lint2 -c ${QP_ROOT}/src/Integrals_Bielec/integral_bielec.cc
  
  mv integral_bielec.o ${QP_ROOT}/src/Integrals_Bielec/
}

BUILD=_build/${TARGET}
rm -rf -- ${BUILD}
mkdir ${BUILD} || exit 1
tar -zxf Downloads/${TARGET}.tgz --strip-components=1 --directory=${BUILD} || exit 1
_install || exit 1
rm -rf -- ${BUILD} _build/${TARGET}.log
exit 0


