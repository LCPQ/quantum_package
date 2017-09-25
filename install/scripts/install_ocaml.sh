#!/bin/bash

cd ..
QP_ROOT=$PWD
cd -

# Normal installation
PACKAGES="core cryptokit.1.10 ocamlfind sexplib ZMQ ppx_sexp_conv ppx_deriving"

# Needed for ZeroMQ
export C_INCLUDE_PATH="${QP_ROOT}"/include:"${C_INCLUDE_PATH}"
export LIBRARY_PATH="${QP_ROOT}"/lib:"${LIBRARY_PATH}"
export LD_LIBRARY_PATH="${QP_ROOT}"/lib:"${LD_LIBRARY_PATH}"

# return 0 if program version is equal or greater than check version
check_version () {
    if [[ $1 == $2 ]]
    then
        return 0
    fi
    local IFS=.
    local i ver1=($1) ver2=($2)
    # fill empty fields in ver1 with zeros
    for ((i=${#ver1[@]}; i<${#ver2[@]}; i++))
    do
        ver1[i]=0
    done
    for ((i=0; i<${#ver1[@]}; i++))
    do
        if [[ -z ${ver2[i]} ]]
        then
            # fill empty fields in ver2 with zeros
            ver2[i]=0
        fi
        if ((10#${ver1[i]} > 10#${ver2[i]}))
        then
            return 1
        fi
        if ((10#${ver1[i]} < 10#${ver2[i]}))
        then
            return 2
        fi
    done
    return 0
}


i=$(gcc -dumpversion)

check_version 4.6 $i 
if [[ $? == 1 ]]
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

echo N | ./ocaml.sh ${QP_ROOT}/bin/ 4.04.2 || exit 1

${QP_ROOT}/bin/opam config setup -a -q || exit 1

export LD_LIBRARY_PATH=${QP_ROOT}/lib:${LD_LIBRARY_PATH}
export LIBRARY_PATH=${QP_ROOT}/lib:${LIBRARY_PATH}
export C_INCLUDE_PATH=${QP_ROOT}/lib:${QP_ROOT}/include:${C_INCLUDE_PATH}
source ${HOME}/.opam/opam-init/init.sh > /dev/null 2> /dev/null || true


NCPUs=$(cat /proc/cpuinfo  | grep -i  MHz | wc -l)
${QP_ROOT}/bin/opam install -j ${NCPUs} stdint.0.4.2 -y -q || exit 1
${QP_ROOT}/bin/opam install -j ${NCPUs} ${PACKAGES} -y -q || exit 1

rm -f ../_build/ocaml.log

