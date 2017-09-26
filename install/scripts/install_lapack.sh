#!/bin/bash -x

git clone https://github.com/Reference-LAPACK/lapack-release.git
cd lapack-release
cp make.inc.example make.inc
make -j 8 
mv librefblas.a liblapack.a libtmglib.a $QP_ROOT/lib



