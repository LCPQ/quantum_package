#!/bin/bash
#
# Installs docopt
# lundi 27 avril 2015, 16:51:34 (UTC+0200)

DOCOPT="docopt.py"
DOCOPT_URL="https://raw.githubusercontent.com/docopt/docopt/master/${DOCOPT}"

if [[ -z ${QPACKAGE_ROOT} ]]
then
  echo "The QPACKAGE_ROOT environment variable is not set."
  echo "Please reload the quantum_package.rc file."
  exit -1
fi

cd ${QPACKAGE_ROOT}

rm -f -- scripts/${DOCOPT}{,c}
${QPACKAGE_ROOT}/scripts/install/fetch_from_web.py ${DOCOPT_URL} ${DOCOPT}

mv ${DOCOPT} scripts/${DOCOPT}
