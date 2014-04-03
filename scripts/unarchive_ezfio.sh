#!/bin/bash
#
# Reads a *.ezfio.md5 file, then fetches the proper data in the
# cache and re-creates the EZFIO database.
# Fri Apr  4 01:02:53 CEST 2014

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
fi

EZFIO_FILE=$(basename $(basename ${1} .md5))

if [[ -z $1 ]]
then
  echo "Usage: $(basename $0) <EZFIO_FILE.md5>"
  exit 1
fi

if [[ ! -f ${EZFIO_FILE}.md5 ]]
then
  echo "Error: ${EZFIO_FILE}.md5 does not exist."
  exit 1
fi

key=($(cut -d ' ' -f 1 Ne_dz.ezfio.md5 ))
filename=($(cut -d ' ' -f 2 Ne_dz.ezfio.md5 ))

for i in ${!key[@]}
do
  MD5=${key[$i]}
  file=${filename[$i]}
  mkdir -p $(dirname $file)
  cp ${QPACKAGE_ROOT}/data/cache/${MD5} ${file}
done
