#!/bin/bash
#
# Setup script. Downloads dependencies if they are not already present
# in the current installation.
# Thu Oct 23 22:02:08 CEST 2014

BLUE="[34m"
RED="[31m"
BLACK="(B[m"

QPACKAGE_ROOT="$( cd "$(dirname "$BASH_SOURCE")"  ; pwd -P )"


if [[ -z "${IRPF90}" ]] ;
then
    make irpf90
    IRPF90="${QPACKAGE_ROOT}"/bin/irpf90
    if [[ ! -x "${IRPF90}" ]]
    then
      echo $RED "Error in IRPF90 installation" $BLACK
      exit 1
    fi
fi


if [[ -z ${OCAMLBREW_BASE} ]]
then
  export OCAMLBREW_BASE="$HOME/ocamlbrew"
fi

cat << EOF > quantum_package.rc
export IRPF90="${IRPF90}"
export QPACKAGE_ROOT=\$( cd \$(dirname "\${BASH_SOURCE}")  ; pwd -P )
export LD_LIBRARY_PATH="\${QPACKAGE_ROOT}"/lib:\${LD_LIBRARY_PATH}
export LIBRARY_PATH="\${QPACKAGE_ROOT}"/lib:\${LIBRARY_PATH}
export C_INCLUDE_PATH="\${QPACKAGE_ROOT}"/include:\${C_INCLUDE_PATH}
export PYTHONPATH=\${PYTHONPATH}:"\${QPACKAGE_ROOT}"/scripts
export PATH=\${PATH}:"\${QPACKAGE_ROOT}"/scripts
export PATH=\${PATH}:"\${QPACKAGE_ROOT}"/bin
export PATH=\${PATH}:"\${QPACKAGE_ROOT}"/ocaml
source "\${QPACKAGE_ROOT}"/bin/irpman &> /dev/null
EOF

source quantum_package.rc
make EZFIO
if [[ ! -d ${QPACKAGE_ROOT}/EZFIO ]]
then
  echo $RED "Error in EZFIO installation" $BLACK
  exit 1
fi

make ocaml
if [[ ! -f ${QPACKAGE_ROOT}/ocaml/Qptypes.ml ]]
then
  echo $RED "Error in ocaml installation" $BLACK
  exit 1
fi

make resultsFile
if [[ ! -d ${QPACKAGE_ROOT}/resultsFile ]]
then
  echo $RED "Error in resultsFile installation" $BLACK
  exit 1
fi


echo $RED "
=======================================================
To complete the installation, add the following line to
your ~/.bashrc:

source ${QPACKAGE_ROOT}/quantum_package.rc

=======================================================
" $BLACK


if [[ $1 == "--robot" ]] ; 
then
  exit 0
else
  source quantum_package.rc
  exec bash
fi

