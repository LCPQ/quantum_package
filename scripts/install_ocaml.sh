#!/bin/bash
#
# Downloads and installs ocaml, opam and core library
# Thu Oct 23 21:58:40 CEST 2014

PACKAGES="core cryptokit"

# Check the QPACKAGE_ROOT directory
if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit 1
fi

cd ${QPACKAGE_ROOT}


function asksure() {
  echo -n "Are you sure (Y/N)? "
  return $retval
}

if [[ -f quantum_package.rc ]]
then
  source quantum_package.rc
fi
make -C ocaml Qptypes.ml &> /dev/null
if [[ $? -ne 0 ]]
then

  if [[ -d ${OCAMLBREW_BASE} ]]
  then
    echo "Remove directory ${OCAMLBREW_BASE} ? [Y/n]"
    while read -r -n 1 -s answer; do
      if [[ $answer = [YyNn] ]]; then
         [[ $answer = [Yy] ]] && rm -rf -- ${HOME}/ocamlbrew
         [[ $answer = [Nn] ]] && exit 1
        break
      fi
    done
    echo $answer
  fi
  scripts/fetch_from_web.py "https://raw.github.com/hcarty/ocamlbrew/master/ocamlbrew-install" ocamlbrew-install.sh 
  cat < ocamlbrew-install.sh | env OCAMLBREW_FLAGS="-r" bash | tee ocamlbrew_install.log
  grep "source " ocamlbrew_install.log | grep "etc/ocamlbrew.bashrc"  >> quantum_package.rc
  source quantum_package.rc
  echo Y | opam install ${PACKAGES}
fi
make -C ocaml Qptypes.ml


