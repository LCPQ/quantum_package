#!/bin/bash
#
# Downloads and installs ocaml, opam and core library
# Thu Oct 23 21:58:40 CEST 2014

PACKAGES="core cryptokit"
OPAM_BASE=$HOME/.opam

# Check the QPACKAGE_ROOT directory
if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit 1
fi

cd ${QPACKAGE_ROOT}


if [[ -f quantum_package.rc ]]
then
  source quantum_package.rc
fi
make -C ocaml Qptypes.ml &> /dev/null

if [[ $? -ne 0 ]]
then

  if [[ -d ${OPAM_BASE} ]]
  then
    echo "${OPAM_BASE} exists."
    echo  "Re-install ? [y/N]"
    while read -r -n 1 -s answer; do
      if [[ $answer = [YyNn] ]]; then
         if [[ $answer = [Yy] ]] ; then
           echo "  - Remove your directory ${OPAM_BASE}"
           echo "  - Restart setup"
           exit 1
         fi
         if [[ $answer = [Nn] ]] ; then
           make -C ocaml Qptypes.ml
           exit 0
         fi
      fi
    done
  fi
fi
${QPACKAGE_ROOT}/scripts/fetch_from_web.py \
   "https://raw.github.com/ocaml/opam/master/shell/opam_installer.sh" opam_installer.sh
chmod +x opam_installer.sh
echo N | ./opam_installer.sh ${QPACKAGE_ROOT}/bin 

if [[ ! -f ${QPACKAGE_ROOT}/bin/opam ]]; then
   echo "Installation of OPAM failed"
   exit 2
fi
rm opam_installer.sh
${QPACKAGE_ROOT}/bin/opam config setup -a --dot-profile ${QPACKAGE_ROOT}/quantum_package.rc 

source ${QPACKAGE_ROOT}/quantum_package.rc
echo Y | opam install ${PACKAGES}

make -C ocaml Qptypes.ml

exit 0 

