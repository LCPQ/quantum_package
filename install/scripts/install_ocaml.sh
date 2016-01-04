#!/bin/bash

cd ..
QP_ROOT=$PWD
cd -

# Normal installation
PACKAGES="core cryptokit ocamlfind sexplib ZMQ"

# Needed for ZeroMQ
export C_INCLUDE_PATH="${QP_ROOT}"/lib:"${C_INCLUDE_PATH}"
export LIBRARY_PATH="${QP_ROOT}"/lib:"${LIBRARY_PATH}"
export LD_LIBRARY_PATH="${QP_ROOT}"/lib:"${LD_LIBRARY_PATH}"

# return 0 if program version is equal or greater than check version
check_version()
{
    local version=$1 check=$2
    local winner=$(echo -e "$version\n$check" | sed '/^$/d' | sort -nr | head -1)
    [[ "$winner" = "$version" ]] && return 0
    return 1
}

i=$(gcc -dumpversion)

if check_version i 4.6
then
   echo "GCC version $(gcc -dumpversion) too old. GCC >= 4.6 required."
   exit 1
fi


if [[ i -eq 4 ]]
then
  i=$(gcc -dumpversion | cut -d '.' -f 2)
  if [[ i -lt 6 ]]
  then
    echo "GCC version $(gcc -dumpversion) too old. GCC >= 4.6 required."
    exit 1
  fi
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


NCPUs=$(cat /proc/cpuinfo  | grep -i  MHz | wc -l)
${QP_ROOT}/bin/opam install -j ${NCPUs} ${PACKAGES} -y -q || exit 1

rm -f ../_build/ocaml.log
exit 0


