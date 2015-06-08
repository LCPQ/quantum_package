#!/bin/bash -x

PACKAGES="core cryptokit ocamlfind sexplib"

declare -i i
i=$(gcc -dumpversion | cut -d '.' -f 2)
if [[ i -lt 6 ]]
then
   echo "GCC version $(gcc -dumpversion) too old. GCC >= 4.6 required."
   exit 1
fi

cd ..
QP_ROOT=$PWD
cd -

cd Downloads || exit 1
chmod +x  ocaml.sh || exit 1

if [[ -d ${HOME}/.opam ]]
then
  source ${HOME}/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

echo N | ./ocaml.sh ${QP_ROOT}/bin/ || exit 1
if [[ ! -f ${QP_ROOT}/bin/opam ]]
then
   echo "Installation of OPAM failed"
   exit 2
fi
${QP_ROOT}/bin/opam config setup -a --dot-profile ${QP_ROOT}/quantum_package.rc  || exit 1

export LD_LIBRARY_PATH=${QP_ROOT}/lib:${LD_LIBRARY_PATH}
export LIBRARY_PATH=${QP_ROOT}/lib:${LIBRARY_PATH}
export C_INCLUDE_PATH=${QP_ROOT}/lib:${C_INCLUDE_PATH}

source ${QP_ROOT}/quantum_package.rc
echo Y | ${QP_ROOT}/bin/opam install ${PACKAGES} || exit 1
rm -f ../_build/ocaml.log
exit 0