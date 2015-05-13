#!/bin/bash 
#
# This script should run from the directory $QPACKAGE_ROOT/src

# Check is we are in `QPROOT_SRC` and all the dependancy are corect
${QPACKAGE_ROOT}/scripts/module/check_src.sh

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

  # Update Makefile.depend
  ${QPACKAGE_ROOT}/scripts/module/check_module.sh

  # Update EZFIO interface (create the irp.f90 and the ocaml)
  ${QPACKAGE_ROOT}/scripts/ezfio_interface/ei_handler.py --irpf90 --ocaml

  # Create symlink
  ${QPACKAGE_ROOT}/scripts/module/module_handler.py create_symlick

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
  # Create gitignore
  ${QPACKAGE_ROOT}/scripts/module/create_gitignore.sh

  # Create png
  ${QPACKAGE_ROOT}/scripts/module/module_handler.py create_png

  # Create png
  ${QPACKAGE_ROOT}/scripts/module/update_README.py

  cd ${OLDPWD}
done
${QPACKAGE_ROOT}/scripts/module/create_executables_list.sh
