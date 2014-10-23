#!/bin/bash
#
# Downloads and installs ocaml, opam and core library
# Thu Oct 23 21:58:40 CEST 2014

QPACKAGE_ROOT=${PWD}

make -C ocaml Qptypes.ml &> /dev/null
if [[ $? -ne 0 ]]
then

  scripts/fetch_from_web.py "https://raw.github.com/hcarty/ocamlbrew/master/ocamlbrew-install" ocamlbrew-install.sh
  cat < ocamlbrew-install.sh | env OCAMLBREW_FLAGS="-r" bash 
  grep "source " ocaml_install.log | grep "etc/ocamlbrew.bashrc"  >> quantum_package.rc
  source quantum_package.rc
  echo Y | opam install core 
fi
make -C ocaml Qptypes.ml


