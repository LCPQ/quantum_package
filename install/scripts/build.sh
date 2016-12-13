#!/bin/bash -x
# This script should be included

BUILD=_build/${TARGET}
rm -rf -- ${BUILD}
mkdir ${BUILD} || exit 1
if [[ -f Downloads/${TARGET}.tar.gz ]] ; then
  tar -zxf Downloads/${TARGET}.tar.gz --strip-components=1 --directory=${BUILD} || exit 1
elif  [[ -f Downloads/${TARGET}.tar.bz2 ]] ; then
  tar -jxf Downloads/${TARGET}.tar.bz2 --strip-components=1 --directory=${BUILD} || exit 1
fi
_install || exit 1
rm -rf -- ${BUILD} _build/${TARGET}.log
exit 0
