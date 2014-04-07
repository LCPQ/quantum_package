#!/bin/bash
#
# Computes the MD5 digest of an EZFIO file, and creates a tar.
# Thu Apr  3 16:55:50 CEST 2014

if [[ -z ${QPACKAGE_ROOT} ]]
then
  print "The QPACKAGE_ROOT environment variable is not set."
  print "Please reload the quantum_package.rc file."
fi

function archive()
{
  FILE=$1
  MD5=$2
  ARCHIVE=${QPACKAGE_ROOT}/data/cache/$MD5
  if [[ -f $ARCHIVE ]]
  then
    if ! diff $FILE ${QPACKAGE_ROOT}/data/cache/$MD5 &> /dev/null
    then
      echo "Something went wrong. The file"
      echo ${QPACKAGE_ROOT}/data/cache/$MD5
      echo "is different from $FILE"
      echo "Aborting"
      return 1
    fi
  else
    cp $FILE ${QPACKAGE_ROOT}/data/cache/$MD5
  fi
}




EZFIO_FILE=$(basename ${1})

if [[ -z ${EZFIO_FILE} ]]
then
  echo "Usage: $(basename $0) <EZFIO_FILE>"
  exit 1
fi


cd ${QPACKAGE_ROOT}/EZFIO/src
FILES=($(python << EOF | sort
from read_config import * 
for group in groups:
  if group == "ezfio":
    continue
  for d in groups[group]:
    if d[2] == ():
      suffix = ""
    else:
      suffix = ".gz"
    print group+'/'+d[0]+suffix
print "ezfio/creation"
print "ezfio/library"
EOF
))
cd $OLDPWD

MD5_FILE=$(basename ${EZFIO_FILE} .ezfio).md5
rm -f ${MD5_FILE}
for FILE in ${FILES[@]}
do
  FILE=${EZFIO_FILE}/${FILE}
  MD5=$(md5sum ${FILE} 2>/dev/null | cut -d ' ' -f 1)
  if [[ ! -z $MD5 ]]
  then
    if ! archive $FILE $MD5
    then
      rm ${MD5_FILE}
      exit 1
    fi
    echo $MD5 $FILE >> ${MD5_FILE}
  fi
done

