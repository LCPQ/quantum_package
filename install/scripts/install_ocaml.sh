#!/bin/bash

cd ..
QP_ROOT=$PWD
cd -

# Fast installation
if [[ "$1" == "--fast" && ! -d ${HOME}/.opam ]]
then
  MEGA_DL="${QP_ROOT}/bin/mega-dl.sh"
  wget 'https://gist.githubusercontent.com/scemama/b9debaed0b76321229a5/raw/d06ca00e6ad7f3703ff4738b210f6f913c1bd8d5/mega-dl.sh' -O ${MEGA_DL}
  chmod +x ${MEGA_DL}
  url='https://mega.co.nz/#!ykh32ajD!2aeqh87c53tL-Z4W1msQvem-TrmRN3ftlZ_QdhKo3c4'
  ${MEGA_DL} $url /tmp/opam.tgz
  cd $HOME
  tar -zxf /tmp/opam.tgz
  cd -
fi

# Normal installation
PACKAGES="core cryptokit ocamlfind sexplib"

declare -i i
i=$(gcc -dumpversion | cut -d '.' -f 2)
if [[ i -lt 6 ]]
then
   echo "GCC version $(gcc -dumpversion) too old. GCC >= 4.6 required."
   exit 1
fi

if [[ -d ${HOME}/.opam ]]
then
  source ${HOME}/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true
fi

cd Downloads || exit 1
chmod +x  ocaml.sh || exit 1

echo N | ./ocaml.sh ${QP_ROOT}/bin/ || exit 1

${QP_ROOT}/bin/opam config setup -a -q || exit 1

export LD_LIBRARY_PATH=${QP_ROOT}/lib:${LD_LIBRARY_PATH}
export LIBRARY_PATH=${QP_ROOT}/lib:${LIBRARY_PATH}
export C_INCLUDE_PATH=${QP_ROOT}/lib:${C_INCLUDE_PATH}
source ${HOME}/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true

${QP_ROOT}/bin/opam install ${PACKAGES} -y -q || exit 1
rm -f ../_build/ocaml.log
exit 0


