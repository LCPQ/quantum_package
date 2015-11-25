#!/bin/bash -x
# This script should be included

BUILD=_build/${TARGET}
rm -rf -- ${BUILD}
mkdir ${BUILD} || exit 1
tar -zxf Downloads/${TARGET}.tar.gz --strip-components=1 --directory=${BUILD} || exit 1
_install
if [[ $? -ne 0 ]]
then
  cat _build/${TARGET}.log
  exit 1
fi
rm -rf -- ${BUILD} _build/${TARGET}.log
exit 0
