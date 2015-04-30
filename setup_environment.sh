#!/bin/bash
#
# Setup script. Downloads dependencies if they are not already present
# in the current installation.
# Thu Oct 23 22:02:08 CEST 2014

BLUE="[34m"
RED="[31m"
BLACK="(B[m"

QPACKAGE_ROOT="$( cd "$(dirname "$BASH_SOURCE")"  ; pwd -P )"


cat << EOF > quantum_package.rc
export IRPF90="${IRPF90}"
export QPACKAGE_ROOT=\$( cd \$(dirname "\${BASH_SOURCE}")  ; pwd -P )
export LD_LIBRARY_PATH="\${QPACKAGE_ROOT}"/lib:\${LD_LIBRARY_PATH}
export LIBRARY_PATH="\${QPACKAGE_ROOT}"/lib:\${LIBRARY_PATH}
export C_INCLUDE_PATH="\${QPACKAGE_ROOT}"/include:\${C_INCLUDE_PATH}

export PYTHONPATH=${PYTHONPATH}$(find "${QPACKAGE_ROOT}"/scripts -type d -printf ":%p")

export PATH=${PATH}$(find "${QPACKAGE_ROOT}"/scripts -type d -printf ":%p")
export PATH=\${PATH}:"\${QPACKAGE_ROOT}"/bin
export PATH=\${PATH}:"\${QPACKAGE_ROOT}"/ocaml
source "\${QPACKAGE_ROOT}"/bin/irpman &> /dev/null
EOF


source quantum_package.rc

echo "${BLUE}===== Installing IRPF90 ===== ${BLACK}"
${QPACKAGE_ROOT}/scripts/install/install_irpf90.sh     | tee install_irpf90.log
if [[ ! -d ${QPACKAGE_ROOT}/irpf90 ]]
then
  echo $RED "Error in IRPF90 installation" $BLACK
  exit 1
fi

if [[ ! -x ${QPACKAGE_ROOT}/bin/irpf90 ]]
then
  echo $RED "Error in IRPF90 installation" $BLACK
  exit 1
fi

if [[ ! -x ${QPACKAGE_ROOT}/bin/irpman ]]
then
  echo $RED "Error in IRPF90 installation" $BLACK
  exit 1
fi


echo "${BLUE}===== Installing Zlib ===== ${BLACK}"
${QPACKAGE_ROOT}/scripts/install/install_zlib.sh       | tee install_zlib.log

echo "${BLUE}===== Installing Curl ===== ${BLACK}"
${QPACKAGE_ROOT}/scripts/install/install_curl.sh       | tee install_curl.log

echo "${BLUE}===== Installing M4 ===== ${BLACK}"
${QPACKAGE_ROOT}/scripts/install/install_m4.sh         | tee install_m4.log

echo "${BLUE}===== Installing Docopt ===== ${BLACK}"
${QPACKAGE_ROOT}/scripts/install/install_docopt.sh     | tee install_docopt.log

echo "${BLUE}===== Installing EMSL Basis set library ===== ${BLACK}"
${QPACKAGE_ROOT}/scripts/install/install_emsl.sh       | tee install_emsl.log

if [[ ! -d ${QPACKAGE_ROOT}/EMSL_Basis ]]
then
  echo $RED "Error in EMSL Basis set library installation" $BLACK
  exit 1
fi

echo "${BLUE}===== Installing EZFIO ===== ${BLACK}"

${QPACKAGE_ROOT}/scripts/install/install_ezfio.sh | tee install_ezfio.log
if [[ ! -d ${QPACKAGE_ROOT}/EZFIO ]]
then
  echo $RED "Error in EZFIO installation" $BLACK
  exit 1
fi


echo "${BLUE}===== Installing Ocaml compiler and libraries ===== ${BLACK}"
rm -f -- ocaml/Qptypes.ml
${QPACKAGE_ROOT}/scripts/install/install_ocaml.sh | tee install_ocaml.log

if [[ ! -f ${QPACKAGE_ROOT}/ocaml/Qptypes.ml ]]
then
  echo $RED "Error in Ocaml installation" $BLACK
  exit 1
fi

echo "${BLUE}===== Installing resultsFile Python library ===== ${BLACK}"
${QPACKAGE_ROOT}/scripts/install/install_resultsFile.sh
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


mkdir -p ${QPACKAGE_ROOT}/install_logs
mv ${QPACKAGE_ROOT}/*.log ${QPACKAGE_ROOT}/install_logs/

if [[ $1 == "--robot" ]] ; 
then
  exit 0
else
  source quantum_package.rc
  exec bash
fi

