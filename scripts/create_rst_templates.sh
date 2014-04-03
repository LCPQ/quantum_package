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
  answer=w
  while [[ $answer != [YyNn] ]]
  do
     read answer
     [[ $answer = [Yy] ]] && retval=0 || retval=1
  done
  return $retval
}

README="True"
if [[ -f README.rst ]]
then
  asksure "Overwrite existing README.rst file?" || README="False"
fi

UNDERLINE="======="
declare -i i=0
while [[ i -lt ${#MODULE} ]]
do
  UNDERLINE+="="
  i+=1
done

if [[ ${README} == "True" ]]
then
cat << EOF > ./README.rst
$UNDERLINE
$MODULE Module
$UNDERLINE

EOF
fi

touch ./ASSUMPTIONS.rst

