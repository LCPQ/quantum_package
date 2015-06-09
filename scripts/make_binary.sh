#!/bin/bash
# 
# Creates a self-contained binary distribution in the form of a tar.gz file
# 
# Tue Jan 13 14:06:25 CET 2015
#


# Check the QP_ROOT directory
if [[ -z ${QP_ROOT} ]]
then
  echo "The QP_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit 1
fi

cd ${QP_ROOT}
if [[ -f quantum_package.rc \
   && -f README.md \
   && -d src  \
   && -d bin  \
   && -d ocaml \
   && -d scripts ]]
then
  head -1 README.md | grep "Quantum package" > /dev/null
  if [[ $? -ne 0 ]]
  then
    echo "This doesn't look like a quantum_package directory  (README.md)"
    exit 1
  fi
else
  echo "This doesn't look like a quantum_package directory"
  exit 1
fi


# Build all sources
for dir in ${QP_ROOT}/{src,ocaml}
do
  make -C ${dir}
  if [[ $? -ne 0 ]]
  then
    echo "Error building ${dir}"
  fi
done


# Copy the files in the static directory
QPACKAGE_STATIC=${QP_ROOT}/quantum_package_static

function find_libs ()
{
  for i in $@
  do
      ldd $i
  done | sort | grep '/' | cut -d ' ' -f 3 | uniq
}

function find_exec ()
{
  find ${QP_ROOT}/$1 -perm /u+x -type f
}



#
echo "Creating root of static directory"
#     ---------------------------------

rm -rf -- ${QPACKAGE_STATIC}
mkdir -p -- ${QPACKAGE_STATIC}/{bin,lib,extra_lib,data}
if [[ $? -ne 0 ]] ;
then
  echo "Error creating ${QPACKAGE_STATIC}/{bin,lib,extra_lib,data}"
  exit 1
fi


#
echo "Copying binary files"
#     --------------------

FORTRAN_EXEC=$(find_exec src)
if [[ -z $FORTRAN_EXEC ]] ;
then
  echo 'Error : No Fortran binaries found.'
  exit 1
fi

OCAML_EXEC=$(find_exec ocaml | grep .native )
if [[ -z $OCAML_EXEC ]] ;
then
  echo 'Error : No ocaml binaries found.'
  exit 1
fi

cp -- ${FORTRAN_EXEC} ${OCAML_EXEC} ${QPACKAGE_STATIC}/bin
if [[ $? -ne 0 ]] ;
then
  echo 'cp -- ${FORTRAN_EXEC} ${OCAML_EXEC} ${QPACKAGE_STATIC}/bin'
  exit 1
fi

cd ${QPACKAGE_STATIC}/bin
  for i in *.native
  do
    mv "$i" $(basename "$i" .native)
  done
cd -

for i in ${FORTRAN_EXEC}
do
  i=$(basename $i)
  echo $i \$QP_ROOT/bin/$i
done >> ${QPACKAGE_STATIC}/data/executables

cp ${QP_ROOT}/data/ezfio_defaults ${QPACKAGE_STATIC}/data
mkdir -p ${QPACKAGE_STATIC}/src/Bitmask
cp ${QP_ROOT}/src/Bitmask/bitmasks_module.f90 ${QPACKAGE_STATIC}/src/Bitmask

#
echo "Copying dynamic libraries"
#     --------------------------

MKL_LIBS=$(find_libs ${FORTRAN_EXEC} | grep libmkl | head -1)
if [[ -n ${MKL_LIBS} ]]
then
  MKL_LIBS=$(dirname ${MKL_LIBS})
  MKL_LIBS=$(ls ${MKL_LIBS}/libmkl_{def,avx,avx2}.so)
fi

ALL_LIBS=$(find_libs ${OCAML_EXEC} ${FORTRAN_EXEC})
cp -- ${ALL_LIBS} ${MKL_LIBS} ${QPACKAGE_STATIC}/extra_lib
if [[ $? -ne 0 ]] ;
then
  echo 'cp -- ${ALL_LIBS} ${MKL_LIBS} ${QPACKAGE_STATIC}/extra_lib'
  exit 1
fi

cp -- ${QPACKAGE_STATIC}/extra_lib/lib{[gi]omp*,mkl*,lapack*,blas*,z*} ${QPACKAGE_STATIC}/lib/

#
echo "Copying EMSL_Basis directory"
#     ---------------------------- 

cp -r -- ${QP_ROOT}/EMSL_Basis ${QPACKAGE_STATIC}/
if [[ $? -ne 0 ]] ;
then
  echo 'cp -r -- ${QP_ROOT}/EMSL_Basis ${QPACKAGE_STATIC}/'
  exit 1
fi


#
echo "Copying scripts directory"
#     ------------------------- 

cp -r -- ${QP_ROOT}/scripts ${QPACKAGE_STATIC}/
if [[ $? -ne 0 ]] ;
then
  echo 'cp -r -- ${QP_ROOT}/scripts ${QPACKAGE_STATIC}/'
  exit 1
fi


#
echo "Creating quantum_package.rc"
#     ---------------------------

cat << EOF > ${QPACKAGE_STATIC}/quantum_package.rc
export QP_ROOT=\$( cd \$(dirname "\${BASH_SOURCE}")  ; pwd -P )
export LD_LIBRARY_PATH="\${QP_ROOT}"/lib:\${LD_LIBRARY_PATH}
export LIBRARY_PATH="\${QP_ROOT}"/lib:\${LIBRARY_PATH}
export PYTHONPATH="\${QP_ROOT}"/scripts:\${PYTHONPATH}
export PATH="\${QP_ROOT}"/scripts:\${PATH}
export PATH="\${QP_ROOT}"/bin:\${PATH}
export PATH="\${QP_ROOT}"/ocaml:\${PATH}
EOF

#exit 0
#
echo "Creating the archive"
#     --------------------

tar -zcf "${QPACKAGE_STATIC}".tar.gz quantum_package_static && rm -rf -- "${QPACKAGE_STATIC}"
if [[ $? -ne 0 ]] ;
then
  echo 'tar -zcf "${QPACKAGE_STATIC}".tar.gz "${QPACKAGE_STATIC}" && rm -rf -- "${QPACKAGE_STATIC}"'
  exit 1
fi

echo "Done : ${QPACKAGE_STATIC}.tar.gz"

