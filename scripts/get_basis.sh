#!/bin/bash
#
# get_basis.sh <temp_file> <basis_name> <atom_list>...
#
# Uses the EMSL_api.py to get the basis set: 
#  https://github.com/TApplencourt/EMSL_Basis_Set_Exchange_Local
#
# Prints in stdout the name of a temporary file containing the basis set.
#

#DEBUG:
#echo $0 $@ 1>&2

if [[ -z ${QP_ROOT} ]]
then
  print "The QP_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
  exit -1
fi


export EMSL_API_ROOT="${QP_ROOT}"/install/emsl

tmpfile="$1"
shift

# Case insensitive basis in input
#basis=$( ${EMSL_API_ROOT}/EMSL_api.py list_basis | cut -d "'" -f 2 | grep -i "^${1}\$")

basis="$1"
shift

atoms=""

for atom in $@
do
  atoms+="--atom ${atom} "
done

if [[ $? -ne 0 ]]
then
  echo "==================================================" 1>& 2
  echo "Unable to create temporary file $tmpfile" 1>& 2
  echo "==================================================" 1>& 2
  echo "ERROR"
  exit 1
fi

${EMSL_API_ROOT}/EMSL_api.py get_basis_data --treat_l --save --path="${tmpfile}" --basis="${basis}"
