#!/bin/bash
#
# Creates the rst files when creating a new module.
# Thu Apr  3 11:54:16 CEST 2014

MODULE=$(basename $PWD)

if [[ $MODULE == "src" ]]
then
  echo "Error: This script should not be run in the src directory."
  exit 1
fi

function asksure() {
  echo -n $@ "(Y/N) "
  while read -r -n 1 -s answer; do
    if [[ $answer = [YyNn] ]]; then
    [[ $answer = [Yy] ]] && retval=0
    [[ $answer = [Nn] ]] && retval=1
      break
    fi
  done
  return $retval
}

if [[ -f README.rst ]]
then
  asksure "Overwrite existing README.rst file?" || exit 1
fi

UNDERLINE="======="
declare -i i=0
while [[ i -lt ${#MODULE} ]]
do
  UNDERLINE+="="
  i+=1
done

cat << EOF > README.rst
$UNDERLINE
$MODULE Module
$UNDERLINE



Assumptions
-----------

.. include:: ./ASSUMPTIONS.rst


Needed Modules
--------------

.. include:: ./NEEDED_MODULES

EOF

