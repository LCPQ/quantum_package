#!/bin/bash 
#
# This script should run from the directory $QPACKAGE_ROOT/src

${QPACKAGE_ROOT}/scripts/check_src.sh

NPROC=$(cat /proc/cpuinfo | grep MHz | wc -l)

export IN_MAKE=1

for MODULE in $@
do
  if [[ ! -d ${MODULE} ]]
  then
    error "Module ${MODULE} doesn't exist"
  fi
  cd ${MODULE}
  echo ${MODULE}  
  ${QPACKAGE_ROOT}/scripts/check_module.sh
  if [[ $# -eq 1 ]] 
  then
    env make -j ${NPROC} all 
  else
    env make -j ${NPROC} all &> make.log 
    if [[ $? -ne 0 ]]
    then
      cat make.log
      error "
Build failed for module $MODULE
"
    fi
  fi
  ${QPACKAGE_ROOT}/scripts/module/create_gitignore.sh
  cd ${OLDPWD}
done
${QPACKAGE_ROOT}/scripts/module/create_executables_list.sh
