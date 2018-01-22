#!/bin/bash

OCAML_VERSION="4.06.0"
PACKAGES="core.v0.10.0 cryptokit ocamlfind sexplib.v0.10.0 ZMQ ppx_sexp_conv ppx_deriving"

if [[ -z ${QP_ROOT} ]]
then
  print "The QP_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi

cd $QP_ROOT/ocaml
opam update
opam switch ${OCAML_VERSION}
eval `opam config env`
opam install -y ${PACKAGES} || echo "Upgrade failed. You can try running
configure ; $0"

