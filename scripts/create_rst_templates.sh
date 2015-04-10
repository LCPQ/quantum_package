#!/bin/bash
#
# Creates the rst files when creating a new module.
# Thu Apr  3 11:54:16 CEST 2014

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi
source ${QPACKAGE_ROOT}/scripts/qp_include.sh

check_current_dir_is_module
MODULE=$(basename $PWD)


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

